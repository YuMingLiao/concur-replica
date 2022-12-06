{-# Language OverloadedStrings #-}
module Concur.Replica.Run where

import           Concur.Core                     (SuspendF(StepView, StepIO, StepBlock, StepSTM, Forever), Widget, step)

import           Control.Monad.Free              (Free(Pure, Free))
import           Control.Concurrent.STM          (atomically)

import qualified Data.Text                       as T

import           Replica.VDOM                    (fireEvent, defaultIndex)
import           Replica.VDOM.Types              (DOMEvent(DOMEvent), HTML)

import           Network.WebSockets.Connection   (ConnectionOptions, defaultConnectionOptions)
import           Network.Wai                     (Middleware)
import qualified Network.Wai.Handler.Replica     as R
import qualified Replica.Types                   as R
import qualified Replica.Application             as R
import qualified Network.Wai.Handler.Warp        as W
import Chronos.Types as Ch
import Control.Monad.IO.Class (liftIO)
import Colog.Core
import Data.Text.IO as TIO
import Replica.Log (Log, format)
import Debug.Trace

logAction :: LogAction IO (Time, Log)
logAction = LogAction $ \(x,y) -> liftIO $ TIO.putStrLn $ format (x,y)

run :: Int -> HTML -> ConnectionOptions -> Middleware -> Widget HTML a -> IO ()
run port index connectionOptions middleware widget
  = R.app (R.Config "" index connectionOptions middleware logAction (minute 5) (minute 5) (liftIO (pure (step widget))) (liftIO <$> stepWidget) ) (W.run port) 

sec n = Ch.Timespan (n * 1000000000)
minute n = Ch.Timespan (n * 1000000000 * 60)

runDefault :: Int -> T.Text -> Widget HTML a -> IO ()
runDefault port title widget
  = R.app (R.Config title [] defaultConnectionOptions id logAction (minute 5) (minute 5) (liftIO (pure (step widget))) (liftIO <$> stepWidget) ) (W.run port) 

stepWidget :: Free (SuspendF HTML) a -> IO (Maybe (HTML, Free (SuspendF HTML) a, IO ()))
stepWidget v = case trace "stepWidget v" v of
  Pure a                   -> trace "Pure a" $ pure Nothing
  Free (StepView new next) -> trace "StepView" $ pure (Just (new, next, pure ()))
  Free (StepIO io next)    -> trace "StepIO" $ io >>= stepWidget . next 
  Free (StepBlock io next) -> trace "StepBlock" $ io >>= stepWidget . next
  Free (StepSTM stm next)  -> trace "StepSTM" $ atomically stm >>= stepWidget . next 
  Free Forever             -> trace "Forever" $ pure Nothing 



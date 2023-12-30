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

compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 g f x y = g (f x y)

run :: Int -> T.Text -> HTML -> ConnectionOptions -> Middleware -> (R.Context -> Widget HTML a) -> IO ()
run port title header connectionOptions middleware widget
  = R.app (R.Config title header connectionOptions middleware logAction (minute 5) (minute 5) (liftIO . pure $ step <$> widget) (liftIO `compose2` stepWidget) ) (W.run port) 

sec n = Ch.Timespan (n * 1000000000)
minute n = Ch.Timespan (n * 1000000000 * 60)

runDefault :: Int -> T.Text -> (R.Context -> Widget HTML a) -> IO ()
runDefault port title widget
  = R.app (R.Config title [] defaultConnectionOptions id logAction (minute 5) (minute 5) (liftIO . pure $ (step <$> widget)) (liftIO `compose2` stepWidget) ) (W.run port) 

stepWidget :: R.Context -> (R.Context -> Free (SuspendF HTML) a) -> IO (Maybe (HTML, R.Context -> Free (SuspendF HTML) a, IO ()))
stepWidget ctx v = case (v ctx) of
  Pure a                   -> pure Nothing
  Free (StepView new next) -> pure (Just (new, const next , pure ()))
  Free (StepIO io next)    -> io >>= stepWidget ctx . \r _ -> next r 
  Free (StepBlock io next) -> io >>= stepWidget ctx . \r _ -> next r
  Free (StepSTM stm next)  -> atomically stm >>= stepWidget ctx . \r _ -> next r
  Free Forever             -> pure Nothing 



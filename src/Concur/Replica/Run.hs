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
import qualified Network.Wai.Handler.Warp        as W

noSession :: R.Context -> IO ()
noSession = \_ -> pure ()

run :: Int -> HTML -> ConnectionOptions -> Middleware -> (R.Context -> Widget HTML a) -> (R.Context -> IO session) -> IO ()
run port index connectionOptions middleware widget getSession
  = W.run port
  $ R.app index connectionOptions middleware (pure (step <$> widget)) getSession stepWidget

runDefault :: Int -> T.Text -> (R.Context -> Widget HTML a) -> IO ()
runDefault port title widget
  = W.run port
  $ R.app (defaultIndex title []) defaultConnectionOptions id (pure (step <$> widget)) noSession stepWidget

-- It's an interpreter for the computation.
-- | No need to use this directly if you're using 'run' or 'runDefault'.
stepWidget :: R.Context -> (R.Context -> Free (SuspendF HTML) a) -> b -> IO (Maybe HTML, R.Event -> Maybe (IO ()), IO (Maybe (R.Context -> Free (SuspendF HTML) a)))
stepWidget ctx v sess = case v ctx of
  Pure a                   -> pure (Nothing, const Nothing, pure Nothing)
  Free (StepView new next) -> pure $ (Just new
                                     ,\event -> fireEvent new (R.evtPath event) (R.evtType event) (DOMEvent $ R.evtEvent event) 
                                     ,pure (Just (const next)))
  Free (StepIO io next)    -> io >>= (\v -> stepWidget ctx v sess)  . \r _ -> next r 
  Free (StepBlock io next) -> io >>= (\v -> stepWidget ctx v sess) . \r _ -> next r 
  Free (StepSTM stm next)  -> atomically stm >>= (\v -> stepWidget ctx v sess) . \r _ -> next r
  Free Forever             -> pure (Nothing, const Nothing, pure Nothing)



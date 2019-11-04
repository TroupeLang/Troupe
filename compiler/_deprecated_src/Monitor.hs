{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Monitor (MonitorT, runMonitor, programCounter, transition, Event (..))
where

import LabelModel
import Control.Monad.State.Lazy as State
import Data.Map.Strict
import Control.Monad.Except


type MonitorState = ( [Label],  Label )
data MonError = Fail

newtype MonitorT m a = Monitor {
  runMon :: ExceptT MonError (StateT MonitorState m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState MonitorState)

instance MonadTrans MonitorT where
  lift v = Monitor $ lift $ lift $ v

runMonitor :: Monad m => MonitorT m b -> m (Either MonError b)
runMonitor v = do
  evalStateT (runExceptT (runMon v)) initState

data Event
  = Branch Label
  | Join
  | Disclose Label

programCounter :: (Monad s) => MonitorT s Label
programCounter = do
  (_, pc ) <- get
  return pc

initState :: MonitorState
initState = ([bottom],bottom )

transition :: (Monad s) => Event -> MonitorT s ()

transition (Disclose l) = do
  (pcstack, pc ) <- get
  if (pc `lub` l) <= bottom
    then return ()
    else Monitor $ throwError Fail

transition (Branch l) = do
  (pcstack, oldpc) <- get
  let newpc = oldpc `lub` l
  put (oldpc:pcstack, newpc)

transition (Join) = do
  (pc:pcstack, _) <- get
  put (pcstack, pc)

module LEIA.Logging
    ( withLog
    , LogEffect
    , LogCasterIOC
    , runLogEffect
    , debug
    , info
    ) where

import Control.Concurrent
import Control.Effect
import Control.Effect.Carrier
import Control.Effect.State
import Control.Effect.Writer
import Control.Monad
import Control.Monad.IO.Class
import GHC.Generics (Generic1)
import qualified System.Log.Caster as DistCaster

setupLog :: IO (DistCaster.LogQueue, IO ())
setupLog = do
    (lq, chan) <- liftIO startLogging

    DistCaster.info lq "----------"
    return (lq, threadDelay 100000)

withLog :: (DistCaster.LogQueue -> IO a) -> IO a
withLog action = do
    (lq, finalize) <- setupLog
    result <- action lq
    finalize
    return result

data LogEffect m k
  = Debug String (m k)
  | Info String (m k)
  deriving stock (Functor, Generic1)
  deriving anyclass (HFunctor, Effect)

debug :: (Member LogEffect sig, Carrier sig m) => String -> m ()
debug s = send (Debug s (pure ()))

info :: (Member LogEffect sig, Carrier sig m) => String -> m ()
info s = send (Info s (pure ()))

startLogging :: IO (DistCaster.LogQueue, DistCaster.LogChan)
startLogging = do
    chan <- liftIO DistCaster.newLogChan
    lq <- liftIO DistCaster.newLogQueue

    relayThreadId <- liftIO . forkIO $ do
          DistCaster.relayLog chan DistCaster.LogInfo DistCaster.stdoutListener
          putStrLn "End relayLog"

    broadcastThreadId <- liftIO . forkIO $ do
          DistCaster.broadcastLog lq chan
          putStrLn "End broadcastLog"
    return (lq, chan)

runLogEffect :: (MonadIO m) => LogCasterIOC m a -> m a
runLogEffect action = do
    (lq, chan) <- liftIO startLogging

    ((liftM snd) . runState (lq, chan) . runLogCasterIOC) $ do
        result <- action
        DistCaster.info lq "----------"
        liftIO $ threadDelay 100000
        return result

newtype LogCasterIOC m a = LogCasterIOC { runLogCasterIOC :: StateC (DistCaster.LogQueue, DistCaster.LogChan) m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (Carrier sig m, Effect sig, MonadIO m) => Carrier (LogEffect :+: sig) (LogCasterIOC m) where
  eff (L (Debug s k)) = LogCasterIOC (dispatchLog DistCaster.debug s) >> k
  eff (L (Info  s k)) = LogCasterIOC (dispatchLog DistCaster.info  s) >> k
  eff (R other)       = LogCasterIOC (eff (R (handleCoercible other)))

dispatchLog :: (Carrier sig m, Effect sig, MonadIO m) => (DistCaster.LogQueue -> String -> IO ()) -> String -> StateC (DistCaster.LogQueue, DistCaster.LogChan) m ()
dispatchLog delegate s = do
  (lq, ch) <- get
  _ <- return (ch :: DistCaster.LogChan)
  liftIO (delegate lq s)
  return ()

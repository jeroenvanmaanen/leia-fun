module LEIA.Logging
    ( withLog
    ) where

import Control.Concurrent
import Control.Effect
import Control.Effect.Carrier
import Control.Effect.State
import Control.Effect.Writer
import Control.Monad.IO.Class
import GHC.Generics (Generic1)
import qualified System.Log.Caster as DistCaster

setupLog :: IO (DistCaster.LogQueue, IO ())
setupLog = do
    chan <- DistCaster.newLogChan
    lq <- DistCaster.newLogQueue

    relayThreadId <- forkIO $ do
          DistCaster.relayLog chan DistCaster.LogInfo DistCaster.stdoutListener
          putStrLn "End relayLog"

    broadcastThreadId <- forkIO $ do
          DistCaster.broadcastLog lq chan
          putStrLn "End broadcastLog"

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

-- startLogging :: IO (DistCaster.LogQueue, DistCaster.LogChan)

runLogEffect :: (MonadIO m) => LogCasterIOC m a -> m ((DistCaster.LogQueue, DistCaster.LogChan), a)
runLogEffect action = do
    (chan, lq) <- liftIO $ do
        chan <- liftIO DistCaster.newLogChan
        lq <- liftIO DistCaster.newLogQueue

        relayThreadId <- liftIO . forkIO $ do
              DistCaster.relayLog chan DistCaster.LogInfo DistCaster.stdoutListener
              putStrLn "End relayLog"

        broadcastThreadId <- liftIO . forkIO $ do
              DistCaster.broadcastLog lq chan
              putStrLn "End broadcastLog"
        return (chan, lq)

    (runState (lq, chan) . runLogCasterIOC) action

    -- DistCaster.info lq "----------"
    -- liftIO $ threadDelay 100000

newtype LogCasterIOC m a = LogCasterIOC { runLogCasterIOC :: StateC (DistCaster.LogQueue, DistCaster.LogChan) m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

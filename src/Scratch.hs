module Scratch
    ( testScratchE
    ) where

import Control.Effect
import Control.Effect.Carrier
import Control.Monad.IO.Class
import Data.IORef
import LEIA.Logging
import System.IO

incRef :: IORef Int -> IO ()
incRef var = do
    val <- readIORef var
    writeIORef var (val+1)

testScratchE :: (Effect sig, Carrier (LogEffect :+: sig) m, MonadIO m) => m ()
testScratchE = do
    val <- liftIO $ do
      var <- newIORef 665
      incRef var
      readIORef var
    info $ show val

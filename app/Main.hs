module Main where

import Santa
import LEIA.LEIA
import LEIA.Logging

import System.IO
import Data.IORef

incRef :: IORef Int -> IO ()
incRef var = do
    val <- readIORef var
    writeIORef var (val+1)

main :: IO ()
main = withLog $ \lq -> do
          var <- newIORef 42
          incRef var
          val <- readIORef var
          print val

          mainSanta lq

          testDelay lq

          someFunc lq

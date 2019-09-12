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

testIncRef :: IO ()
testIncRef = do
    var <- newIORef 42
    incRef var
    val <- readIORef var
    print val

main :: IO ()
main = do
    testIncRef
    withLog $ \lq -> do
        mainSanta lq
        testDelay lq
        someFunc lq

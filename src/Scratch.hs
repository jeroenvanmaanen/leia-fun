module Scratch
    ( testScratch
    ) where

import Data.IORef
import System.IO
import System.Log.Caster

incRef :: IORef Int -> IO ()
incRef var = do
    val <- readIORef var
    writeIORef var (val+1)

testIncRef :: LogQueue -> IO ()
testIncRef lq = do
    var <- newIORef 42
    incRef var
    val <- readIORef var
    info lq val

testScratch :: LogQueue -> IO ()
testScratch = testIncRef

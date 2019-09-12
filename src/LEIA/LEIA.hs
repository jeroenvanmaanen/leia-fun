module LEIA.LEIA
    ( someFunc
    ) where

import System.Log.Caster

someFunc :: LogQueue -> IO ()
someFunc lq = info lq "someFunc"

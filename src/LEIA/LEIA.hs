module LEIA.LEIA
    ( someFunc
    ) where

import Control.Effect
import Control.Effect.Carrier
import Control.Monad.IO.Class
import LEIA.Logging

someFunc :: (Member LogEffect sig, Carrier sig m) => m ()
someFunc = info "someFunc"

module LEIA.LEIA
    ( someFunc
    ) where

import Control.Effect
import Control.Effect.Carrier
import Control.Monad.IO.Class
import LEIA.Logging

someFunc :: (Effect sig, Carrier (LogEffect :+: sig) m, MonadIO m) => m ()
someFunc = info "someFunc"

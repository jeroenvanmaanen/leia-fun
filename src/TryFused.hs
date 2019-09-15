{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- http://hackage.haskell.org/package/fused-effects
-- https://www.youtube.com/watch?v=vfDazZfxlNs "Building Haskell Programs with Fused Effects" by Patrick Thomson
-- https://github.com/fused-effects/fused-effects/blob/master/examples/Teletype.hs

module TryFused
( runTeletypeIO
, tryFused
) where

import Prelude hiding (read)

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.State
import Control.Effect.Writer
import Control.Monad.IO.Class
import GHC.Generics (Generic1)
import System.Log.Caster

tryFused :: LogQueue -> IO ()
tryFused lq = runM . runTeletypeIO $ do
  tryX
  info lq "there!"

tryX :: (MonadIO m, Carrier sig m) => TeletypeIOC m ()
tryX = write "Hi"

data Teletype m k
  = Read (String -> m k)
  | Write String (m k)
  deriving stock (Functor, Generic1)
  deriving anyclass (HFunctor, Effect)

read :: (Member Teletype sig, Carrier sig m) => m String
read = send (Read pure)

write :: (Member Teletype sig, Carrier sig m) => String -> m ()
write s = send (Write s (pure ()))

runTeletypeIO :: TeletypeIOC m a -> m a
runTeletypeIO = runTeletypeIOC

newtype TeletypeIOC m a = TeletypeIOC { runTeletypeIOC :: m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Carrier sig m) => Carrier (Teletype :+: sig) (TeletypeIOC m) where
  eff (L (Read    k)) = liftIO getLine      >>= k
  eff (L (Write s k)) = liftIO (putStrLn s) >>  k
  eff (R other)       = TeletypeIOC (eff (handleCoercible other))


runTeletypeRet :: [String] -> TeletypeRetC m a -> m ([String], ([String], a))
runTeletypeRet i = runWriter . runState i . runTeletypeRetC

newtype TeletypeRetC m a = TeletypeRetC { runTeletypeRetC :: StateC [String] (WriterC [String] m) a }
  deriving newtype (Applicative, Functor, Monad)

instance (Carrier sig m, Effect sig) => Carrier (Teletype :+: sig) (TeletypeRetC m) where
  eff (L (Read    k)) = do
    i <- TeletypeRetC get
    case i of
      []  -> k ""
      h:t -> TeletypeRetC (put t) *> k h
  eff (L (Write s k)) = TeletypeRetC (tell [s]) *> k
  eff (R other)       = TeletypeRetC (eff (R (R (handleCoercible other))))
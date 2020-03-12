{-# LANGUAGE  DeriveFunctor #-}

module CoolQ.HTTP.Api.Monad
  ( ApiT (..)
  , ApiM ) where
import CoolQ.HTTP.Api.Caller
  ( ApiCaller )
import Data.Aeson
  ( Object )
import Control.Concurrent.Chan
  ( Chan )

newtype ApiT o m a = ApiT { runApiT :: (ApiCaller m, Chan o) -> m a }
  deriving (Functor)

instance (Applicative m) => Applicative (ApiT o m) where
  f <*> x = ApiT (\env -> runApiT f env <*> runApiT x env)
  pure x = ApiT (const (pure x))

instance (Monad m) => Monad (ApiT o m) where
  x >>= f = ApiT (\env -> runApiT x env >>= (\x -> runApiT (f x) env))

type ApiM a = ApiT Object IO a

{-# LANGUAGE  DeriveFunctor #-}

module CoolQ.HTTP.Api.Monad
  ( ApiT (..)
  , ApiM ) where
import CoolQ.HTTP.Api.Caller
  ( ApiCaller )

newtype ApiT m a = ApiT { runApiT :: ApiCaller m -> m a }
  deriving (Functor)

instance (Applicative m) => Applicative (ApiT m) where
  f <*> x = ApiT (\env -> runApiT f env <*> runApiT x env)
  pure x = ApiT (const (pure x))

instance (Monad m) => Monad (ApiT m) where
  x >>= f = ApiT (\env -> runApiT x env >>= (\x -> runApiT (f x) env))

type ApiM a = ApiT IO a

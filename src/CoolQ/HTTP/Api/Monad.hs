{-# LANGUAGE  DeriveFunctor #-}

module CoolQ.HTTP.Api.Monad
  ( ApiT
  , ApiM ) where
import CoolQ.HTTP.Api.Caller
  ( ApiCaller )
import Data.Aeson
  ( Object )
import Control.Concurrent.Chan
  ( Chan )
import Control.Monad.IO.Class
  ( MonadIO (liftIO) )
import Control.Monad.Reader
  ( ReaderT )

type ApiT o m a = ReaderT (ApiCaller m, Chan o) m a

type ApiM a = ApiT Object IO a

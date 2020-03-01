{-# LANGUAGE  OverloadedStrings
            , ExistentialQuantification #-}

module CoolQ.HTTP.Event
  ( EventConfig (..)
  , EventHandler
  , listen ) where

import Web.Scotty
import Data.Aeson
  ( Object )
import Data.Text
  ( Text )
import Data.ByteString
  ( ByteString )
import Data.ByteString.Lazy
  ( toStrict )
import Data.Maybe
  ( fromMaybe )
import Data.Functor
  ( (<&>) )
import Crypto.MAC.HMAC
  ( hmac
  , hmacGetDigest )
import Crypto.Hash.Algorithms
  ( SHA1 )
import Crypto.Hash
  ( Digest )
import Control.Monad.IO.Class
  ( liftIO )
import Data.String
  ( fromString )
import Network.HTTP.Types
  ( ok200
  , unauthorized401
  , forbidden403 )
import Control.Concurrent.Async
  ( mapConcurrently )

data EventConfig =
  EventConfig
  { eventPort :: Int
  , eventSecret :: Maybe ByteString }
type EventHandler = Object -> IO ()

listen :: EventConfig -> [EventHandler] -> IO ()
listen conf handlers = scotty (eventPort conf) $
  post "/" $ do
    raw <- body
    rawSig <- header "X-Signature"
    case (eventSecret conf, rawSig) of
      (Just sec, Just reqSig) ->
        let sig = hmacGetDigest (hmac sec (toStrict raw)) :: Digest SHA1
        in if reqSig == fromString (show sig)
          then status ok200
          else status forbidden403 >> finish
      (Just sec, Nothing) ->
        status unauthorized401 >> finish
      (Nothing, _) ->
        status ok200
    raw <- jsonData
    liftIO $ ($ raw) `mapConcurrently` handlers
    finish

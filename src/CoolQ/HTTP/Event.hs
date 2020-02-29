{-# LANGUAGE OverloadedStrings #-}

module CoolQ.HTTP.Event where

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

data EventConfig =
  EventConfig
  { port :: Int
  , secret :: Maybe ByteString }
type EventHandler = Object -> ActionM ()

listen :: EventConfig -> [EventHandler] -> IO ()
listen conf handlers = scotty (port conf) $
  get "/" $ do
    raw <- body
    rawSig <- header "X-Signature"
    case (secret conf, rawSig) of
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
    sequence (($ raw) <$> handlers)
    finish

{-# LANGUAGE OverloadedStrings #-}

module CoolQ.HTTP.Api where

import Network.HTTP.Req
import Data.Aeson
  ( Value )
import Data.Text
  ( Text )
import Data.ByteString
  ( ByteString )
import Data.Maybe
  ( fromMaybe )
import Data.Functor
  ( (<&>) )

data ApiConfig s =
  ApiConfig
  { host :: Url s
  , hostPort :: Int
  , accessToken :: Maybe ByteString }

api :: MonadHttp m => ApiConfig s -> Text -> Value -> m Value
api conf method payload =
  responseBody <$> res where
  res =
    req POST
    (host conf /: method)
    (ReqBodyJson payload)
    (jsonResponse)
    (authOpt <> portOpt)
  authOpt =
    fromMaybe mempty $
      accessToken conf
      <&> (<>) "Bearer "
      <&> header "Authorization"
  portOpt = port $ hostPort conf

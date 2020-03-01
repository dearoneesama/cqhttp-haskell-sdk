{-# LANGUAGE  OverloadedStrings
            , ExistentialQuantification #-}

module CoolQ.HTTP.Api
  ( ApiConfig (..)
  , ApiCaller
  , api ) where
import Network.HTTP.Req
import Data.Aeson
  ( Value
  , object )
import Data.Text
  ( Text )
import Data.ByteString
  ( ByteString )
import Data.Maybe
  ( fromMaybe )
import Data.Functor
  ( (<&>) )
import Control.Monad.IO.Class
  ( MonadIO )

data ApiConfig s =
  ApiConfig
  { host :: Text
  , hostPort :: Int
  , accessToken :: Maybe ByteString }
type ApiCaller m =
   Text -> Value -> m Value

api :: MonadIO m => HttpConfig -> ApiConfig s -> ApiCaller m
api httpConf conf method payload =
  runReq httpConf $ responseBody <$> res where
  res =
    req POST
    (http (host conf) /: method)
    (ReqBodyJson payload)
    (jsonResponse)
    (authOpt <> portOpt)
  authOpt =
    fromMaybe mempty $
      accessToken conf
      <&> (<>) "Bearer "
      <&> header "Authorization"
  portOpt = port $ hostPort conf

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
import Control.Concurrent.Async
  ( async
  , Async )

data ApiConfig s =
  ApiConfig
  { apiHost :: Text
  , apiPort :: Int
  , apiToken :: Maybe ByteString }
type ApiCaller =
   Text -> Value -> IO (Async Value)

api :: HttpConfig -> ApiConfig s -> ApiCaller
api httpConf conf method payload =
  async $ runReq httpConf $ responseBody <$> res where
  res =
    req POST
    (http (apiHost conf) /: method)
    (ReqBodyJson payload)
    (jsonResponse)
    (authOpt <> portOpt)
  authOpt =
    fromMaybe mempty $
      apiToken conf
      <&> (<>) "Bearer "
      <&> header "Authorization"
  portOpt = port $ apiPort conf

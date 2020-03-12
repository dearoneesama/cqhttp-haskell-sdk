{-# LANGUAGE  OverloadedStrings #-}

module CoolQ.HTTP.Api.Caller
  ( ApiConfig (..)
  , ApiCaller
  , Retcode
  , CQException
  , toInt
  , makeApiCaller ) where
import Network.HTTP.Req
import Data.Aeson
  ( Value (Number, Object)
  , object )
import Data.Text
  ( Text )
import Data.ByteString
  ( ByteString )
import Data.Maybe
  ( fromMaybe )
import Control.Monad.IO.Class
  ( MonadIO )
import Data.HashMap.Strict
  ( (!) )
import Control.Exception
  ( throw
  , Exception )
import Data.Scientific
  ( Scientific
  , toRealFloat )
import Data.Functor
  ( (<&>) )

data ApiConfig =
  ApiConfig
  { apiHost :: Text
  , apiPort :: Int
  , apiToken :: Maybe ByteString }
  deriving (Show, Eq)
type ApiCaller m =
  Text -> [(Text, Value)] -> m Value

type Retcode = Int
newtype CQException = CQException Retcode
  deriving (Show, Eq)
instance Exception CQException

toInt :: Scientific -> Int
toInt = floor . toRealFloat

makeApiCaller :: MonadIO m => HttpConfig -> ApiConfig -> ApiCaller m
makeApiCaller httpConf conf method payload = do
  rawResponse <- runReq httpConf $ responseBody <$> request
  case rawResponse of
    (Object response) ->
      case response!"retcode" of
        (Number 0) -> pure (response!"data")
        (Number x) -> throw (CQException $ toInt x)
        _ -> error "retcode is not a number. this should not happen"
    _ -> error "response is not an object. this should not happen"
  where
    request =
      req POST
      (http (apiHost conf) /: method)
      (ReqBodyJson $ object payload)
      (jsonResponse)
      (authOpt <> portOpt)
    authOpt =
      fromMaybe mempty $
        apiToken conf
        <&> (<>) "Bearer "
        <&> header "Authorization"
    portOpt = port $ apiPort conf

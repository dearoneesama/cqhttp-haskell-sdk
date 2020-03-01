{-# LANGUAGE  OverloadedStrings
            , ExistentialQuantification
            , DeriveGeneric
            , DuplicateRecordFields #-}

module CoolQ.HTTP.Api
  ( ApiConfig (..)
  , ApiCaller ()
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
import Data.Functor
  ( (<&>) )
import Control.Monad.IO.Class
  ( MonadIO )
import Control.Concurrent.Async
  ( async
  , Async )
import Data.Time.Clock
  ( NominalDiffTime )
import Data.HashMap.Strict
  ( (!) )
import Control.Monad.Except
  ( catchError )
import Data.Scientific
  ( toRealFloat )

data ApiConfig =
  ApiConfig
  { apiHost :: Text
  , apiPort :: Int
  , apiToken :: Maybe ByteString }

type Retcode = Int

type Result a = IO (Async (Either Retcode a))

type ApiCaller =
  Text -> Value -> Result Value

makeApiCaller :: HttpConfig -> ApiConfig -> ApiCaller
makeApiCaller httpConf conf method payload = async $ do
  rawResponse <- runReq httpConf $ responseBody <$> request
  case rawResponse of
    (Object response) ->
      case response!"retcode" of
        (Number 0) -> pure $ Right (response!"data")
        (Number x) -> pure $ Left (floor $ toRealFloat x)
        _ -> error "retcode is not a number"
    _ -> error "unexpected response"
  where
    request =
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

{-
-- TODO
sendPrivateMsg :: UserId -> Message -> Result MessageId


sendGroupMsg :: GroupId -> Message -> Result MessageId


sendDiscussMsg :: DiscussId -> Message -> Result MessageId


sendMsg :: MessageType -> Int -> Message -> Result MessageId


deleteMsg :: MessageId -> Result ()


sendLike :: UserId -> Int -> Result ()


setGroupKick :: GroupId -> UserId -> Bool -> Result ()


setGroupBan :: GroupId -> UserId -> NominalDiffTime -> Result ()


unsetGroupBan :: GroupId -> UserId -> Result ()


setGroupAnonymousBan :: GroupId -> AnonymousFlag -> NominalDiffTime -> Result ()


setGroupWholeBan :: GroupId -> Result ()


unsetGroupWholeBan :: GroupId -> Result ()

...
-}

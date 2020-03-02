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
  , object
  , Object
  , (.=) )
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
import Data.HashMap.Strict
  ( (!) )
import Control.Exception
  ( throw
  , catch
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
type ApiCaller =
  Text -> [(Text, Value)] -> IO Object

type Retcode = Int
data CQException = CQException Retcode
  deriving (Show, Eq)
instance Exception CQException

toInt :: Scientific -> Int
toInt = floor . toRealFloat

makeApiCaller :: HttpConfig -> ApiConfig -> ApiCaller
makeApiCaller httpConf conf method payload = do
  rawResponse <- runReq httpConf $ responseBody <$> request
  case rawResponse of
    (Object response) ->
      case response!"retcode" of
        (Number 0) -> return (let (Object obj) = response!"data" in obj)
        (Number x) -> throw (CQException $ toInt x)
        _ -> error "retcode is not a number"
    _ -> error "unexpected response"
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

type UserId = Int
type GroupId = Int
type DiscussId = Int
type MessageId = Int
data MessageType =
  PrivateMessage |
  GroupMessage |
  DiscussMessage
  deriving (Show, Eq)
type AnonymousFlag = Text
type Message = [Value]
type Duration = Int

discard :: Monad m => m a -> m ()
discard = (>> pure ())

sendPrivateMsg :: ApiCaller -> UserId -> Message -> IO MessageId
sendPrivateMsg api user msg = do
  (Number id) <- raw <&> (!"message_id")
  pure $ toInt id
  where
    raw = api "send_private_msg"
      [ "user_id" .= user
      , "message" .= msg ]

sendGroupMsg :: ApiCaller -> GroupId -> Message -> IO MessageId
sendGroupMsg api group msg = do
  (Number id) <- raw <&> (!"message_id")
  pure $ toInt id
  where
    raw = api "send_group_msg"
      [ "group_id" .= group
      , "message" .= msg ]

sendDiscussMsg :: ApiCaller -> DiscussId -> Message -> IO MessageId
sendDiscussMsg api discuss msg = do
  (Number id) <- raw <&> (!"message_id")
  pure $ toInt id
  where
    raw = api "send_discuss_msg"
      [ "discuss_id" .= discuss
      , "message" .= msg ]

sendMsg :: ApiCaller -> MessageType -> Int -> Message -> IO MessageId
sendMsg api typ id msg = do
  (Number id) <- raw <&> (!"message_id")
  pure $ toInt id
  where
    raw = api "send_msg"
      [ "message" .= msg
      , (case typ of
          PrivateMessage -> "user_id"
          DiscussMessage -> "discuss_id"
          GroupMessage -> "group_id") .= id]

deleteMsg :: ApiCaller -> MessageId -> IO ()
deleteMsg api id = discard $
  api "delete_msg"
    [ "message_id" .= id ]

sendLike :: ApiCaller -> UserId -> Int-> IO ()
sendLike api user times = discard $
  api "send_like"
    [ "user_id" .= user
    , "times" .= times ]

setGroupKick :: ApiCaller -> GroupId -> UserId -> Bool -> IO ()
setGroupKick api group user rejectAddRequest = discard $
  api "set_group_kick"
    [ "group_id" .= group
    , "user_id" .= user
    , "reject_add_request" .= rejectAddRequest ]

setGroupBan :: ApiCaller -> GroupId -> UserId -> Duration -> IO ()
setGroupBan api group user duration = discard $
  api "set_group_ban"
    [ "group_id" .= group
    , "user_id" .= user
    , "duration" .= duration ]

unsetGroupBan :: ApiCaller -> GroupId -> UserId -> IO ()
unsetGroupBan api group user =
  setGroupBan api group user 0

setGroupAnonymousBan :: ApiCaller -> GroupId -> AnonymousFlag -> Duration -> IO ()
setGroupAnonymousBan api group flag duration = discard $
  api "set_group_anonymous_ban"
    [ "group_id" .= group
    , "anonymous_flag" .= flag
    , "duration" .= duration ]

switchGroupWholeBan :: ApiCaller -> GroupId -> Bool -> IO ()
switchGroupWholeBan api group enable = discard $
  api "set_group_whole_ban"
    [ "group_id" .= group
    , "enable" .= enable ]

setGroupWholeBan :: ApiCaller -> GroupId -> IO ()
setGroupWholeBan api group =
  switchGroupWholeBan api group True

unsetGroupWholeBan :: ApiCaller -> GroupId -> IO ()
unsetGroupWholeBan api group =
  switchGroupWholeBan api group False

switchGroupAdmin :: ApiCaller -> GroupId -> UserId -> Bool -> IO ()
switchGroupAdmin api group user enable = discard $
  api "set_group_admin"
    [ "group_id" .= group
    , "user_id" .= user
    , "enable" .= enable]

setGroupAdmin :: ApiCaller -> GroupId -> UserId -> IO ()
setGroupAdmin api group user =
  switchGroupAdmin api group user True

unsetGroupAdmin :: ApiCaller -> GroupId -> UserId -> IO ()
unsetGroupAdmin api group user =
  switchGroupAdmin api group user False

switchGroupAnonymous :: ApiCaller -> GroupId -> Bool -> IO ()
switchGroupAnonymous api group enable = discard $
  api "set_group_anonymous"
    [ "group_id" .= group
    , "enable" .= enable]

setGroupAnonymous :: ApiCaller -> GroupId -> IO ()
setGroupAnonymous api group =
  switchGroupAnonymous api group True

unsetGroupAnonymous :: ApiCaller -> GroupId -> IO ()
unsetGroupAnonymous api group =
  switchGroupAnonymous api group False

setGroupCard :: ApiCaller -> GroupId -> UserId -> Text -> IO ()
setGroupCard api group user card = discard $
  api "set_group_card"
    [ "group_id" .= group
    , "user_id" .= user
    , "card" .= card ]

setGroupLeave :: ApiCaller -> GroupId -> IO ()
setGroupLeave api group = discard $
  api "set_group_leave"
    [ "group_id" .= group ]

setGroupDismiss :: ApiCaller -> GroupId -> IO ()
setGroupDismiss api group = discard $
  api "set_group_leave"
    [ "group_id" .= group
    , "is_dismiss" .= True ]

setGroupSpecialTitle :: ApiCaller -> GroupId -> UserId -> Text -> IO ()
setGroupSpecialTitle api group user title = discard $
  api "set_group_special_title"
    [ "group_id" .= group
    , "user_id" .= user
    , "special_title" .= title ]

setDiscussLeave :: ApiCaller -> DiscussId -> IO ()
setDiscussLeave api discuss = discard $
  api "set_discuss_leave"
    [ "discuss_id" .= discuss ]

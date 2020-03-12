{-# LANGUAGE  OverloadedStrings
            , DeriveGeneric
            , DuplicateRecordFields #-}

module CoolQ.HTTP.Api
  ( ApiConfig (..)
  , ApiCaller
  , Retcode
  , CQException
  , makeApiCaller
  , ApiT (..)
  , ApiM
  , UserId
  , GroupId
  , DiscussId
  , MessageId
  , MessageType (..)
  , GroupRequestType (..)
  , AnonymousFlag
  , RequestFlag
  , Message
  , Duration
  , callApi
  , sendPrivateMsg
  , sendGroupMsg
  , sendDiscussMsg
  , sendMsg
  , deleteMsg
  , sendLike
  , setGroupKick
  , setGroupBan
  , unsetGroupBan
  , setGroupAnonymousBan
  , switchGroupWholeBan
  , setGroupWholeBan
  , unsetGroupWholeBan
  , switchGroupAdmin
  , setGroupAdmin
  , unsetGroupAdmin
  , switchGroupAnonymous
  , setGroupAnonymous
  , unsetGroupAnonymous
  , setGroupCard
  , setGroupLeave
  , setGroupDismiss
  , setGroupSpecialTitle
  , setDiscussLeave
  , setFriendAddRequest
  , approveFriendAddRequest
  , rejectFriendAddRequest
  , setGroupAddRequest
  , approveGroupAddRequest
  , rejectGroupAddRequest
  , LoginInfo (..)
  , getLoginInfo
  , StrangerInfo (..)
  , getStrangerInfo
  , getUncachedStrangerInfo ) where
import CoolQ.HTTP.Api.Caller
import CoolQ.HTTP.Api.Monad
import GHC.Generics
  ( Generic )
import Data.Aeson
  ( Value (Number, Object)
  , (.=)
  , FromJSON
  , fromJSON
  , Result (Success) )
import Data.Text
  ( Text )
import Control.Monad.IO.Class
  ( MonadIO )
import Data.HashMap.Strict
  ( (!) )

type UserId = Int
type GroupId = Int
type DiscussId = Int
type MessageId = Int
data MessageType =
  PrivateMessage |
  GroupMessage |
  DiscussMessage
  deriving (Show, Eq)
data GroupRequestType =
  GroupAddRequest |
  GroupInviteRequest
  deriving (Show, Eq)
type AnonymousFlag = Text
type RequestFlag = Text
type Message = [Value]
type Duration = Int

discard :: Monad m => m a -> m ()
discard = (>> pure ())

liftedToInt :: Monad m => m Value -> m Int
liftedToInt val = do
  uval <- val
  case uval of
    (Number sci) -> pure (toInt sci)
    _ -> error "The data is not a number. This should not happen"

liftedIndex :: Monad m => Text -> m Value -> m Value
liftedIndex idx val = do
  uval <- val
  case uval of
    (Object obj) -> pure (obj!idx)
    _ -> error "The data is not an object. This should not happen"

liftedFromJSON :: (Monad m, FromJSON a) => m Value -> m a
liftedFromJSON val = do
  uval <- fromJSON <$> val
  case uval of
    (Success obj) -> pure obj
    _ -> error "The data is not an object. This should not happen"

callApi :: ApiCaller (ApiT o m)
callApi method payload = ApiT $ \(api, _) ->
  api method payload

sendPrivateMsg :: MonadIO m => UserId -> Message -> ApiT o m MessageId
sendPrivateMsg user msg = liftedToInt $ liftedIndex "message_id" $
  callApi "send_private_msg"
    [ "user_id" .= user
    , "message" .= msg ]

sendGroupMsg :: MonadIO m => GroupId -> Message -> ApiT o m MessageId
sendGroupMsg group msg = liftedToInt $ liftedIndex "message_id" $
  callApi "send_group_msg"
    [ "group_id" .= group
    , "message" .= msg ]

sendDiscussMsg :: MonadIO m => DiscussId -> Message -> ApiT o m MessageId
sendDiscussMsg discuss msg = liftedToInt $ liftedIndex "message_id" $
  callApi "send_discuss_msg"
    [ "discuss_id" .= discuss
    , "message" .= msg ]

sendMsg :: MonadIO m => MessageType -> Int -> Message -> ApiT o m MessageId
sendMsg typ id msg = liftedToInt $ liftedIndex "message_id" $
  callApi "send_msg"
    [ "message" .= msg
    , (case typ of
        PrivateMessage -> "user_id"
        DiscussMessage -> "discuss_id"
        GroupMessage -> "group_id") .= id]

deleteMsg :: MonadIO m => MessageId -> ApiT o m ()
deleteMsg id = discard $
  callApi "delete_msg"
    [ "message_id" .= id ]

sendLike :: MonadIO m => UserId -> Int -> ApiT o m ()
sendLike user times = discard $
  callApi "send_like"
    [ "user_id" .= user
    , "times" .= times ]

setGroupKick :: MonadIO m => GroupId -> UserId -> Bool -> ApiT o m ()
setGroupKick group user rejectAddRequest = discard $
  callApi "set_group_kick"
    [ "group_id" .= group
    , "user_id" .= user
    , "reject_add_request" .= rejectAddRequest ]

setGroupBan :: MonadIO m => GroupId -> UserId -> Duration -> ApiT o m ()
setGroupBan group user duration = discard $
  callApi "set_group_ban"
    [ "group_id" .= group
    , "user_id" .= user
    , "duration" .= duration ]

unsetGroupBan :: MonadIO m => GroupId -> UserId -> ApiT o m ()
unsetGroupBan group user =
  setGroupBan group user 0

setGroupAnonymousBan :: MonadIO m => GroupId -> AnonymousFlag -> Duration -> ApiT o m ()
setGroupAnonymousBan group flag duration = discard $
  callApi "set_group_anonymous_ban"
    [ "group_id" .= group
    , "anonymous_flag" .= flag
    , "duration" .= duration ]

switchGroupWholeBan :: MonadIO m => GroupId -> Bool -> ApiT o m ()
switchGroupWholeBan group enable = discard $
  callApi "set_group_whole_ban"
    [ "group_id" .= group
    , "enable" .= enable ]

setGroupWholeBan :: MonadIO m => GroupId -> ApiT o m ()
setGroupWholeBan group =
  switchGroupWholeBan group True

unsetGroupWholeBan :: MonadIO m => GroupId -> ApiT o m ()
unsetGroupWholeBan group =
  switchGroupWholeBan group False

switchGroupAdmin :: MonadIO m => GroupId -> UserId -> Bool -> ApiT o m ()
switchGroupAdmin group user enable = discard $
  callApi "set_group_admin"
    [ "group_id" .= group
    , "user_id" .= user
    , "enable" .= enable]

setGroupAdmin :: MonadIO m => GroupId -> UserId -> ApiT o m ()
setGroupAdmin group user =
  switchGroupAdmin group user True

unsetGroupAdmin :: MonadIO m => GroupId -> UserId -> ApiT o m ()
unsetGroupAdmin group user =
  switchGroupAdmin group user False

switchGroupAnonymous :: MonadIO m => GroupId -> Bool -> ApiT o m ()
switchGroupAnonymous group enable = discard $
  callApi "set_group_anonymous"
    [ "group_id" .= group
    , "enable" .= enable]

setGroupAnonymous :: MonadIO m => GroupId -> ApiT o m ()
setGroupAnonymous group =
  switchGroupAnonymous group True

unsetGroupAnonymous :: MonadIO m => GroupId -> ApiT o m ()
unsetGroupAnonymous group =
  switchGroupAnonymous group False

setGroupCard :: MonadIO m => GroupId -> UserId -> Text -> ApiT o m ()
setGroupCard group user card = discard $
  callApi "set_group_card"
    [ "group_id" .= group
    , "user_id" .= user
    , "card" .= card ]

setGroupLeave :: MonadIO m => GroupId -> ApiT o m ()
setGroupLeave group = discard $
  callApi "set_group_leave"
    [ "group_id" .= group ]

setGroupDismiss :: MonadIO m => GroupId -> ApiT o m ()
setGroupDismiss group = discard $
  callApi "set_group_leave"
    [ "group_id" .= group
    , "is_dismiss" .= True ]

setGroupSpecialTitle :: MonadIO m => GroupId -> UserId -> Text -> ApiT o m ()
setGroupSpecialTitle group user title = discard $
  callApi "set_group_special_title"
    [ "group_id" .= group
    , "user_id" .= user
    , "special_title" .= title ]

setDiscussLeave :: MonadIO m => DiscussId -> ApiT o m ()
setDiscussLeave discuss = discard $
  callApi "set_discuss_leave"
    [ "discuss_id" .= discuss ]

setFriendAddRequest :: MonadIO m => RequestFlag -> Bool -> Text -> ApiT o m ()
setFriendAddRequest flag approve remark = discard $
  callApi "set_friend_add_request"
    [ "flag" .= flag
    , "approve" .= approve
    , "remark" .= remark ]

approveFriendAddRequest :: MonadIO m => RequestFlag -> ApiT o m ()
approveFriendAddRequest flag =
  setFriendAddRequest flag True ""

rejectFriendAddRequest :: MonadIO m => RequestFlag -> ApiT o m ()
rejectFriendAddRequest flag =
  setFriendAddRequest flag False ""

setGroupAddRequest :: MonadIO m => GroupRequestType -> RequestFlag -> Bool -> Text -> ApiT o m ()
setGroupAddRequest typ flag approve reason = discard $
  callApi "set_group_add_request"
    [ "type" .=
        (case typ of
          GroupAddRequest -> "add" :: Text
          GroupInviteRequest -> "invite" :: Text)
    , "flag" .= flag
    , "approve" .= approve
    , "reson" .= reason ]

approveGroupAddRequest :: MonadIO m => GroupRequestType -> RequestFlag -> ApiT o m ()
approveGroupAddRequest typ flag =
  setGroupAddRequest typ flag True ""

rejectGroupAddRequest :: MonadIO m => GroupRequestType -> RequestFlag -> Text -> ApiT o m ()
rejectGroupAddRequest typ flag reason =
  setGroupAddRequest typ flag False reason

data LoginInfo =
  LoginInfo
  { user_id :: UserId
  , nickname :: Text }
  deriving (Show, Eq, Generic)
instance FromJSON LoginInfo

getLoginInfo :: MonadIO m => ApiT o m LoginInfo
getLoginInfo = liftedFromJSON $
  callApi "get_login_info" []

data StrangerInfo =
  StrangerInfo
  { user_id :: UserId
  , nickname :: Text
  , sex :: Text
  , age :: Int }
  deriving (Show, Eq, Generic)
instance FromJSON StrangerInfo

_getStrangerInfo :: MonadIO m => UserId -> Bool -> ApiT o m StrangerInfo
_getStrangerInfo user noCache = liftedFromJSON $
  callApi "get_stranger_info"
    [ "user_id" .= user
    , "no_cache" .= noCache ]

getStrangerInfo :: MonadIO m => UserId -> ApiT o m StrangerInfo
getStrangerInfo user =
  _getStrangerInfo user False

getUncachedStrangerInfo :: MonadIO m => UserId -> ApiT o m StrangerInfo
getUncachedStrangerInfo user =
  _getStrangerInfo user True

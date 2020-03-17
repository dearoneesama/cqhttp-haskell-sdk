{-# LANGUAGE  OverloadedStrings
            , DeriveGeneric
            , DuplicateRecordFields
            , LiberalTypeSynonyms #-}

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
import CoolQ.HTTP.Internal.Util
import GHC.Generics
  ( Generic )
import Data.Aeson
  ( Value (Number, Object)
  , (.=)
  , (.:)
  , FromJSON (parseJSON)
  , withObject
  , fromJSON
  , Result (Success) )
import Data.Text
  ( Text )
import Control.Monad.IO.Class
  ( MonadIO )
import Data.HashMap.Strict
  ( (!) )
import Control.Monad.Reader
  ( ReaderT (ReaderT) )
import Data.Time
  ( NominalDiffTime
  , UTCTime )
import Data.Maybe
  ( fromMaybe )

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
data Sex =
  Male |
  Female |
  Unknown
  deriving (Show, Eq)
type AnonymousFlag = Text
type RequestFlag = Text
type Message = [Value]
type Duration = NominalDiffTime

toSex :: Text -> Sex
toSex "male" = Male
toSex "female" = Female
toSex _ = Unknown

discard :: Monad m => m a -> m ()
discard = (>> pure ())

liftedToIntegral :: (Monad m, Integral a) => m Value -> m a
liftedToIntegral val = do
  uval <- val
  case uval of
    (Number sci) -> pure (toIntegral sci)
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
callApi method payload = ReaderT $ \(api, _) ->
  api method payload

sendPrivateMsg :: MonadIO m => UserId -> Message -> ApiT o m MessageId
sendPrivateMsg user msg = liftedToIntegral $ liftedIndex "message_id" $
  callApi "send_private_msg"
    [ "user_id" .= user
    , "message" .= msg ]

sendGroupMsg :: MonadIO m => GroupId -> Message -> ApiT o m MessageId
sendGroupMsg group msg = liftedToIntegral $ liftedIndex "message_id" $
  callApi "send_group_msg"
    [ "group_id" .= group
    , "message" .= msg ]

sendDiscussMsg :: MonadIO m => DiscussId -> Message -> ApiT o m MessageId
sendDiscussMsg discuss msg = liftedToIntegral $ liftedIndex "message_id" $
  callApi "send_discuss_msg"
    [ "discuss_id" .= discuss
    , "message" .= msg ]

sendMsg :: MonadIO m => MessageType -> Int -> Message -> ApiT o m MessageId
sendMsg typ id msg = liftedToIntegral $ liftedIndex "message_id" $
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
    , "duration" .= (toMilliseconds duration :: Int) ]

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
    , "enable" .= enable ]

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
  { userId :: UserId
  , nickname :: Text
  , sex :: Sex
  , age :: Int }
  deriving (Show, Eq, Generic)
instance FromJSON StrangerInfo where
  parseJSON =
    withObject "StrangerInfo" $ \v -> StrangerInfo
      <$> v.:"user_id"
      <*> v.:"nickname"
      <*> (toSex <$> (v.:"sex"))
      <*> v.:"age"

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

data FriendListItem =
  FriendListItem
  { userId :: UserId
  , nickname :: Text
  , remark :: Text }
  deriving (Show, Eq, Generic)
instance FromJSON FriendListItem where
  parseJSON =
    withObject "FriendListItem" $ \v -> FriendListItem
      <$> v.:"user_id"
      <*> v.:"nickname"
      <*> v.:"remark"

getFriendList :: MonadIO m => ApiT o m [FriendListItem]
getFriendList = liftedFromJSON $
  callApi "get_friend_list" []

data GroupListItem =
  GroupListItem
  { groupId :: GroupId
  , groupName :: Text }
  deriving (Show, Eq, Generic)
instance FromJSON GroupListItem where
  parseJSON =
    withObject "GroupListItem" $ \v -> GroupListItem
      <$> v.:"group_id"
      <*> v.:"group_ame"

getGroupList :: MonadIO m => ApiT o m [GroupListItem]
getGroupList = liftedFromJSON $
  callApi "get_group_list" []

data GroupInfo =
  GroupInfo
  { groupId :: GroupId
  , groupName :: Text
  , memberCount :: Int
  , maxMemberCount :: Int }
  deriving (Show, Eq, Generic)
instance FromJSON GroupInfo where
  parseJSON =
    withObject "GroupInfo" $ \v -> GroupInfo
      <$> v.:"group_id"
      <*> v.:"group_name"
      <*> v.:"member_count"
      <*> v.:"max_member_count"

_getGroupInfo :: MonadIO m => UserId -> Bool -> ApiT o m GroupInfo
_getGroupInfo user noCache = liftedFromJSON $
  callApi "get_group_info"
    [ "group_id" .= user
    , "no_cache" .= noCache ]

getGroupInfo :: MonadIO m => UserId -> ApiT o m GroupInfo
getGroupInfo user =
  _getGroupInfo user False

getUncachedGroupInfo :: MonadIO m => UserId -> ApiT o m GroupInfo
getUncachedGroupInfo user =
  _getGroupInfo user True

{-
-- TODO
data GroupMemberInfo =
  GroupMemberInfo
  { groupId :: GroupId
  , userId :: UserId
  , nickname :: Text
  , card :: Text
  , sex :: Sex
  , age :: Int
  , area :: Text
  , joinTime :: UTCTime
  , lastSentTime :: UTCTime
  , level :: Text
  , role :: Text
  , unfriendly :: Bool
  , title :: Text
  , titleExpireTime :: UTCTime
  , cardChangeable :: Bool }
instance FromJSON GroupMemberInfo where
  parseJSON =
    withObject "GroupMemberInfo" $ \v -> GroupMemberInfo
      <$> v.:"group_id"
      <*> v.:"user_id"
      <*> v.:"nickname"
      <*> v.:"card"
      <*> (toSex <$> v.:"sex")
      <*> v.:"age"
      <*> v.:"area"
      <*> v.:"join_time"
-}

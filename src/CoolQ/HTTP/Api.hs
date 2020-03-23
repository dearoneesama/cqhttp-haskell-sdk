{-# LANGUAGE  OverloadedStrings
            , DeriveGeneric
            , DuplicateRecordFields
            , LiberalTypeSynonyms
            , RankNTypes
            , TemplateHaskell
            , DataKinds #-}

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
  , sendPrivateMsg ) where
import CoolQ.HTTP.Api.Caller
import CoolQ.HTTP.Api.Monad
import CoolQ.HTTP.Api.Internal.Util
import CoolQ.HTTP.Api.Internal.Generate
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
type Duration = Int

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


-- These APIs are specially implemented because of the pattern matching.
sendMsg :: MonadIO m => Message -> MessageType -> Int -> ApiT o m MessageId
sendMsg msg typ id = liftedToIntegral $ liftedIndex "message_id" $
  callApi "send_msg"
    [ "message" .= msg
    , (case typ of
        PrivateMessage -> "user_id"
        DiscussMessage -> "discuss_id"
        GroupMessage -> "group_id") .= id]

setGroupAddRequest :: MonadIO m => Bool -> Text -> GroupRequestType -> RequestFlag -> ApiT o m ()
setGroupAddRequest approve reason typ flag = discard $
  callApi "set_group_add_request"
    [ "type" .=
        (case typ of
          GroupAddRequest -> "add" :: Text
          GroupInviteRequest -> "invite" :: Text)
    , "flag" .= flag
    , "approve" .= approve
    , "reson" .= reason ]

$(defineApi [d|

  sendPrivateMsg userId message = liftedToIntegral . liftedIndex "messsage_id"
    :: UserId -> Message -> MessageId

  sendGroupMsg groupId message = liftedToIntegral . liftedIndex "message_id"
    :: GroupId -> Message -> MessageId

  sendDiscussMsg discussId message = liftedToIntegral . liftedIndex "message_id"
    :: DiscussId -> Message -> MessageId

  deleteMsg messageId = discard
    :: MessageId -> ()

  sendLike userId times = discard
    :: UserId -> Int -> ()

  setGroupKick groupId userId rejectAddRequest = discard
    :: GroupId -> UserId -> Bool -> ()

  setGroupBan groupId userId duration = discard
    :: GroupId -> UserId -> Duration -> ()

  setGroupAnonymousBan groupId anonymousFlag duration = discard
    :: GroupId -> AnonymousFlag -> Duration -> ()
  
  setGroupWholeBan groupId enable = discard
    :: GroupId -> Bool -> ()

  setGroupAdmin groupId userId enable = discard
    :: GroupId -> UserId -> Bool -> ()

  setGroupAnonymous groupId enable = discard
    :: GroupId -> Bool -> ()

  setGroupCard groupId userId card = discard
    :: GroupId -> UserId -> Text -> ()

  setGroupLeave groupId isDismiss = discard
    :: GroupId -> Bool -> ()

  setGroupSpecialTitle groupId userId specialTitle = discard
    :: GroupId -> UserId -> Text -> ()

  setDiscussLeave discussId = discard
    :: DiscussId -> ()

  setFriendAddRequest flag approve remark = discard
    :: RequestFlag -> Bool -> Text -> ()

  |])
{-
setGroupAddRequest :: MonadIO m => Bool -> Text -> GroupRequestType -> RequestFlag -> ApiT o m ()
setGroupAddRequest approve reason typ flag = discard $
  callApi "set_group_add_request"
    [ "type" .=
        (case typ of
          GroupAddRequest -> "add" :: Text
          GroupInviteRequest -> "invite" :: Text)
    , "flag" .= flag
    , "approve" .= approve
    , "reson" .= reason ]

approveGroupAddRequest :: MonadIO m => GroupRequestType -> RequestFlag -> ApiT o m ()
approveGroupAddRequest =
  setGroupAddRequest True ""

rejectGroupAddRequest :: MonadIO m => Text -> GroupRequestType -> RequestFlag -> ApiT o m ()
rejectGroupAddRequest =
  setGroupAddRequest False

data LoginInfo =
  LoginInfo
  { user_id :: UserId
  , nickname :: Text }
  deriving (Show, Eq, Generic)
instance FromJSON LoginInfo
{-
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
      <*> (readSex <$> (v.:"sex"))
      <*> v.:"age"

_getStrangerInfo :: MonadIO m => Bool -> UserId -> ApiT o m StrangerInfo
_getStrangerInfo noCache user = liftedFromJSON $
  callApi "get_stranger_info"
    [ "user_id" .= user
    , "no_cache" .= noCache ]

getStrangerInfo :: MonadIO m => UserId -> ApiT o m StrangerInfo
getStrangerInfo =
  _getStrangerInfo False

getUncachedStrangerInfo :: MonadIO m => UserId -> ApiT o m StrangerInfo
getUncachedStrangerInfo =
  _getStrangerInfo True

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

_getGroupInfo :: MonadIO m => Bool -> GroupId -> ApiT o m GroupInfo
_getGroupInfo noCache group = liftedFromJSON $
  callApi "get_group_info"
    [ "group_id" .= group
    , "no_cache" .= noCache ]

getGroupInfo :: MonadIO m => GroupId -> ApiT o m GroupInfo
getGroupInfo =
  _getGroupInfo False

getUncachedGroupInfo :: MonadIO m => GroupId -> ApiT o m GroupInfo
getUncachedGroupInfo =
  _getGroupInfo True

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
  deriving (Show, Eq, Generic)
instance FromJSON GroupMemberInfo where
  parseJSON =
    withObject "GroupMemberInfo" $ \v -> GroupMemberInfo
      <$> v.:"group_id"
      <*> v.:"user_id"
      <*> v.:"nickname"
      <*> v.:"card"
      <*> (readSex <$> v.:"sex")
      <*> v.:"age"
      <*> v.:"area"
      <*> (toUTC' <$> v.:"join_time")
      <*> (toUTC' <$> v.:"last_sent_time")
      <*> v.:"level"
      <*> v.:"role"
      <*> v.:"unfriendly"
      <*> v.:"title"
      <*> (toUTC' <$> v.:"title_expire_time")
      <*> v.:"card_changeable"
    where
    toUTC' :: Int -> UTCTime
    toUTC' = toUTC

_getGroupMemberInfo :: MonadIO m => Bool -> GroupId -> UserId -> ApiT o m GroupMemberInfo
_getGroupMemberInfo noCache group user = liftedFromJSON $
  callApi "get_group_member_info"
    [ "group_id" .= group
    , "user_id" .= user
    , "no_cache" .= noCache ]

getGroupMemberInfo :: MonadIO m => GroupId -> UserId -> ApiT o m GroupMemberInfo
getGroupMemberInfo =
  _getGroupMemberInfo False

getUncachedGroupMemberInfo :: MonadIO m => GroupId -> UserId -> ApiT o m GroupMemberInfo
getUncachedGroupMemberInfo =
  _getGroupMemberInfo True

getGroupMemberList :: MonadIO m => GroupId -> ApiT o m [GroupMemberInfo]
getGroupMemberList group = liftedFromJSON $
  callApi "get_group_member_list"
    [ "group_id" .= group ]

getCookies :: MonadIO m => Text -> ApiT o m Text
getCookies domain = liftedIndex "cookies" $
  callApi "get_cookies"
    [ "domain" .= domain ]

getCsrfToken :: MonadIO m => ApiT o m Text
getCsrfToken = liftedIndex "token" $
  callApi "get_csrf_token" []

data RecordOutFormat =
  Mp3 |
  Amr |
  Wma |
  M4a |
  Spx |
  Ogg |
  Wav |
  Flac
  deriving (Eq)

getRecord :: MonadIO m => Bool -> RecordOutFormat -> Text -> ApiT o m Text
getRecord fullPath outFormat file = liftedIndex "file" $
  callApi "get_record"
    [ "file" .= file
    , "out_format" .= outFormat ]
-}
-}
# CoolQ HTTP API Haskell SDK

这是 [CoolQ HTTP API](https://github.com/richardchien/coolq-http-api) 在 Haskell 语言下的一个 SDK。

## 进度

- [ ] `CoolQ.HTTP`
  - [ ] api
    - [x] structure
    - [ ] types
  - [ ] events
    - [x] structure
    - [ ] types
- [ ] Tests

> 目前没有支持 WebSocket/反向 WebSocket 的计划，因为太难写了。如果您乐意贡献相关内容，我很欢迎。

## 示例

```haskell
{-# LANGUAGE  OverloadedStrings #-}
import CoolQ.HTTP.Api
import CoolQ.HTTP.Event
import Data.Text (Text)
import Network.HTTP.Req (defaultHttpConfig)
import Data.HashMap.Strict ((!))
import Data.Aeson ((.=), object)

call :: ApiCaller IO
call = api defaultHttpConfig
  (ApiConfig
  { host = "localhost"
  , hostPort = 5700
  , accessToken = Just "my-access-token" })

myListener :: EventHandler
myListener event =
  if event!"post_type" == "message"
  then do
    call "send_private_msg" $ object $
      [ "user_id" .= event!"user_id"
      , "message" .= ("Pong!" :: Text) ]
    pure ()
  else pure ()

main :: IO ()
main = listen
  (EventConfig
  { listenPort = 8080
  , secret = Just "my-secret" })
  [myListener]
```

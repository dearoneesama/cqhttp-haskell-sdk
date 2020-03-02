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

## 异步相关

每个事件监听器都在单独的线程上运行，不会阻塞。
但需要注意的是，API 调用默认不会转发到新线程上执行，也就是说，会阻塞**当前 computation 所在的**线程。
如果需要异步的 API 调用，则可以自己调用 `async` 库。

## 示例

```haskell
{-# LANGUAGE  OverloadedStrings #-}
import CoolQ.HTTP.Api
import CoolQ.HTTP.Event
import Data.Text (Text)
import Network.HTTP.Req (defaultHttpConfig)
import Data.HashMap.Strict ((!))
import Data.Aeson ((.=), object)
import Control.Concurrent.Async (wait)

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
    wait <$> (call "send_private_msg" $ object $
      [ "user_id" .= event!"user_id"
      , "message" .= ("Pong!" :: Text) ])
    pure ()
  else pure ()

main :: IO ()
main = listen
  (EventConfig
  { listenPort = 8080
  , secret = Just "my-secret" })
  [myListener]
```

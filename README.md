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
- [ ] `CoolQ.WS.Client`
  - [ ] api
    - [ ] structure
    - [ ] types
  - [ ] events
    - [ ] structure
    - [ ] types
- [ ] `CoolQ.WS.Server` (ws-reverse)
  - [ ] api
    - [ ] structure
    - [ ] types
  - [ ] events
    - [ ] structure
    - [ ] types

## 示例

```haskell
{-# LANGUAGE OverloadedStrings #-}
import CoolQ.HTTP.Api
import CoolQ.HTTP.Event
import Web.Scotty as S
import Data.HashMap.Strict ((!))
import Data.Aeson ((.=))

myListener :: EventHandler
myListener event = do
  if event!"post_type" == "message"
    then S.json ["reply" .= "Hi!"]
    else pure ()

main :: IO ()
main = listen
  (EventConfig
  { port = 8080
  , secret = "my-secret" })
  [myListener]
```

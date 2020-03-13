# CoolQ HTTP API Haskell SDK

这是 [CoolQ HTTP API](https://github.com/richardchien/coolq-http-api) 在 Haskell 语言下的一个 SDK。

## 限制

### WebSocket API 相关
目前没有支持 WebSocket/反向 WebSocket 的计划，因为太难写了。
如果您乐意贡献相关内容，我很欢迎。

### 异步相关
每个事件监听器都在单独的线程上运行，不会阻塞。
但需要注意的是，API 调用默认不会转发到新线程上执行，也就是说，会阻塞**当前 computation 所在的**线程。
如果需要异步的 API 调用，则可以自己调用 `lifted-async` 库的 `Control.Concurrent.Async.Lifted.Safe` 模块
（不能直接使用 `async` 库，因为我们使用 `ReaderT .. IO` 而非裸的 `IO`）。

## 进度

- [ ] `CoolQ.HTTP`
  - [ ] api
    - [x] structure
    - [ ] types
  - [ ] events
    - [x] structure
    - [ ] types
- [ ] Tests

## 示例

> TODO

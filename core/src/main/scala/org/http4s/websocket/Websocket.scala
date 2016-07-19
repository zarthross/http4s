package org.http4s.websocket

#+scalaz-stream
import scalaz.concurrent.Task
import scalaz.stream.Process
#-scalaz-stream
#+fs2
import fs2.{Stream => Process, Task}
#-fs2

import org.http4s.websocket.WebsocketBits.WebSocketFrame

private[http4s] final case class Websocket(
  source: Process[Task, WebSocketFrame],
  sink: Process[Task, WebSocketFrame]
)


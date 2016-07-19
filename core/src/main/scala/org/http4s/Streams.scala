package org.http4s

import java.io.OutputStream

/** Compatibility layer between scalaz-stream and fs2 */
object streams {
#+scalaz-stream
  import scalaz.concurrent.Task
  import scalaz.io._
  import scalaz.stream._

  def chunkW(out: => OutputStream) =
    io.chunkW(out)
#-scalaz-stream

#+fs2
  import fs2._
  import fs2.interop.scalaz._

  def chunkW(out: => OutputStream) =
    fs2.io.file.writeOutputStream[Task](Task.delay(out))

  implicit class StreamCompatSyntax[F[_], A](s: Stream[F, A]) {
    def kill: Stream[F, A] = s.open.close
  }
#-fs2
}

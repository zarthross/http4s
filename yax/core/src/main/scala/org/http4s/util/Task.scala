package org.http4s.util

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Success, Failure}
import scalaz.{-\/, \/-}
import scalaz.syntax.either._

#+scalaz-stream
import scalaz.concurrent.Task
#-scalaz-stream
#+fs2
import fs2.Task
#-fs2

trait TaskFunctions {
  def unsafeTaskToFuture[A](task: Task[A]): Future[A] = {
    val p = Promise[A]()
#+scalaz-stream
    task.runAsync(_.fold(p.failure, p.success))
#-scalaz-stream
#+fs2
    task.unsafeRunAsync(_.fold(p.failure, p.success))
#-fs2
    p.future
  }

  def futureToTask[A](f: => Future[A])(implicit ec: ExecutionContext): Task[A] = {
#+fs2
    implicit val s = fs2.Strategy.fromExecutionContext(ec)
#-fs2
    Task.async { cb =>
      f.onComplete {
        case Success(a) =>
#+scalaz-stream
          cb(a.right)
#-scalaz-stream
#+fs2
          cb(Right(a))
#-fs2
        case Failure(t) => 
#+scalaz-stream
          cb(t.left)
#-scalaz-stream
#+fs2
          cb(Left(t))
#-fs2
      }
    }
  }
}

object task extends TaskFunctions

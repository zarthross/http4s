package org.http4s.examples

import org.http4s.scalatra.ScalatraService
import org.http4s.Status
import scalaz.{State, Monad, Unapply}
import effectful._
import scalaz._

object ScalatraExample extends ScalatraService {
  GET("/gone") {
    status(Status.Gone)
    s"status is ${status}"
  }

  // TODO figure out how to get the macro to infer an Action where there is no effect
  GET("/pure") {
    State.state("pure").asInstanceOf[ScalatraService.Action[String]].!
  }
}
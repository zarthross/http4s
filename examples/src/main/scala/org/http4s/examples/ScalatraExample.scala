package org.http4s.examples

import org.http4s.{RequestPrelude, Request, Status}
import org.http4s.scalatra.ScalatraService
import effectful._

object ScalatraExample extends ScalatraService {
  GET("/gone") {
    status(Status.Gone).!
    "Ross has taken leave of his senses"
  }
}
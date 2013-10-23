package org.http4s.examples

import org.http4s._
import org.http4s.scalatra._

object ScalatraExample extends ScalatraService {
  get("/gone") {
    status = Status.Gone
    status.reason
  }

  getM("/gone2") (for {
    _ <- setStatus(Status.Gone)
    s <- getStatus
  } yield s.reason)

  get("/pure") { "pure" }

  // Banned at compile time
  // status
}

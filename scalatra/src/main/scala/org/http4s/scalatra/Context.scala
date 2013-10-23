package org.http4s.scalatra

import org.http4s.{Response, Request}

case class Context(req: Request, params: Map[String, Seq[String]] = Map.empty, res: Response = Response())

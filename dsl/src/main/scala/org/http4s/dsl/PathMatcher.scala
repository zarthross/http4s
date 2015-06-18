package org.http4s
package dsl

import java.util.regex.Pattern

import scala.util.matching.Regex

class PathMatcher(sc: StringContext) {
  def unapplySeq(path: String): Option[Seq[String]] = {
    val regex = (sc.parts.tail.map {
      case part if part.startsWith("*") =>
        // splat suffix on previous capture
        "(.*)" + Pattern.quote(part.substring(1))
      case part =>
        "([^/]*)" + Pattern.quote(part)
    }).mkString(
      Pattern.quote(sc.parts.head),
      "",
      ""
    ).r
    regex.unapplySeq(path)
  }

  def unapplySeq(req: Request): Option[Seq[String]] =
    unapplySeq(req.pathInfo)
}

object PathMatcher {
  def apply(sc: StringContext): PathMatcher = {
    new PathMatcher(sc)
  }
}

class RequestMatcher(method: Method, sc: StringContext) {
  def unapplySeq(req: Request): Option[Seq[String]] =
    if (req.method == method)
      new PathMatcher(sc).unapplySeq(req.pathInfo)
    else
      None
}

package org.http4s

/**
 * @author Bryce Anderson
 *         Created on 2/22/14
 */
package object scalatra {
  case class Action(path: String, req: Request => Writable[_], params: Map[String, Seq[String]] = Map.empty)
}

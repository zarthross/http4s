package org.http4s

import scala.reflect.internal.annotations.compileTimeOnly
import scalaz.State
import scala.language.experimental.macros
import scala.reflect.macros.{Context => MContext}

package object scalatra {
  type Action[A] = State[Context, A]

  sealed trait CanUnwrap[A]

  @compileTimeOnly("FAIL")
  implicit def canUnwrapImpl[A](implicit a: A): CanUnwrap[A] = new CanUnwrap[A] {}
}

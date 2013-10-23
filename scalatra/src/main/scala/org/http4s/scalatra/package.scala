package org.http4s

import scala.reflect.internal.annotations.compileTimeOnly
import scalaz.State
import scala.language.experimental.macros
import scala.reflect.macros.{Context => MContext}

package object scalatra {
  type Action[A] = State[Context, A]

  trait CanUnwrap[A]

  @compileTimeOnly("FAIL")
  def canUnwrapImpl[A](a: A): CanUnwrap[A] = new CanUnwrap[A] {}

  implicit def canUnwrap[A](implicit a: A): org.http4s.scalatra.CanUnwrap[A] =
    macro canUnwrapMacroImpl[A]

  def canUnwrapMacroImpl[A: c.WeakTypeTag](c: MContext)(a: c.Expr[A]): c.Expr[org.http4s.scalatra.CanUnwrap[A]] =
    c.universe.reify { canUnwrapImpl[A](a.splice) }
}

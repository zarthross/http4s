package org.http4s.rho

import scala.reflect.runtime.universe._

package object compat {
  private[rho] def dealias(t: Type): Type =
    t.dealias

  private[rho] def typeArgs(t: Type): List[Type] =
    t.typeArgs

  private[rho] def primaryConstructor(c: ClassSymbol): Symbol =
    c.primaryConstructor

  private[rho] def paramLists(m: MethodSymbol): List[List[Symbol]] =
    m.paramLists

  private[rho] def CONSTRUCTOR =
    termNames.CONSTRUCTOR
}

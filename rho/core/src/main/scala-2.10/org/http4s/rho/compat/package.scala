package org.http4s.rho

import scala.reflect.runtime.universe._

package object compat {
  private[rho] def dealias(t: Type): Type =
    t.normalize

  private[rho] def typeArgs(t: Type): List[Type] =
    t.asInstanceOf[TypeRefApi].args

  private[rho] def primaryConstructor(c: ClassSymbol): Symbol =
    c.toType.declaration(nme.CONSTRUCTOR).asTerm.alternatives.collectFirst {
      case ctor: MethodSymbol if ctor.isPrimaryConstructor => ctor
    }.get /* questionable taste, but it's guaranteed to exist in 2.11 */

  private[rho] def paramLists(m: MethodSymbol): List[List[Symbol]] =
    m.paramss

  private[rho] def CONSTRUCTOR =
    nme.CONSTRUCTOR
}

package org.http4s.scalatra

import org.http4s.{Method, Request, Writable, HttpService}
import scala.reflect.internal.annotations.compileTimeOnly

import scala.language.experimental.macros

/**
 * @author Bryce Anderson
 *         Created on 2/22/14
 */


object ScalatraServiceMacros {
  import scala.reflect.macros.Context



  def compileRoute[R: c.WeakTypeTag](c: Context { type PrefixType = ScalatraService})
               (method: c.Expr[Method], path: c.Expr[String], result: c.Expr[R]): c.Expr[Unit] = {
    import c.universe._

    val service: c.Expr[ScalatraService] = c.prefix

    println(showRaw(result.tree))
    println(result.tree)

    // the name which will be visible for the param
    val reqName = c.fresh("request")

    val walker = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case t@ q"$a.this.request" => Ident(newTermName(reqName))
        case t => super.transform(t)
      }
    }

    val transformed = c.resetLocalAttrs(walker.transform(result.tree))

    println("After: \n" + transformed)

    val genMethodName = newTermName(c.fresh("genMethod"))
    val genMethodExpr = c.Expr[Request => Unit](Ident(genMethodName))

    val methodBody = c.Expr(q"def $genMethodName(${newTermName(reqName)}: Request) = $transformed")

    val finished = reify {

      methodBody.splice

      service.splice.addRoute(path.splice, method.splice, genMethodExpr.splice)
    }
    println(finished)
    finished
  }

  def getImpl[R: c.WeakTypeTag](c: Context { type PrefixType = ScalatraService })
                 (path: c.Expr[String])(result: c.Expr[R])(writable: c.Expr[Writable[R]]): c.Expr[Unit] = {

    import c.universe._

    compileRoute(c)(reify(Method.Get), path, result)
  }


}

trait ScalatraService {

  import ScalatraServiceMacros._

  def addRoute(path: String, method: Method, route: Request => Unit) {
    println(s"Added route $method: $path")
  }

  @compileTimeOnly("The request field can only be accessed from inside the HTTP method generators!")
  def request: Request = sys.error("Shouldn't get here")

  def get[R](path: String)(result: R)(implicit writable: Writable[R]): Unit = macro getImpl[R]



}

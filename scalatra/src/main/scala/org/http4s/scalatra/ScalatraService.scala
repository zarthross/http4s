package org.http4s.scalatra

import org.http4s._
import scalaz.concurrent.Task
import scala.reflect.macros.{Context => MContext}
import effectful._
import scala.language.experimental.macros
import scalaz._
import org.http4s.Request
import scala.reflect.internal.annotations.compileTimeOnly
import org.http4s.Request
import scala.Some
import org.http4s.Request

object ScalatraService {

  // FIXME Needs to be scoped to service
  var routes: PartialFunction[Request, Task[Response]] = {
    case req =>
      println(req.prelude.pathInfo)
      Status.NotFound()
  }

  def addRoute[A](path: String, action: => Action[A], writable: Writable[A]) {
    val route: PartialFunction[Request, Task[Response]] = {
      case req: Request if req.prelude.pathInfo == path =>
        val (ctx, w) = action.run(Context(req))
        writable.toBody(w).map(body => ctx.res.copy(body = body._1))
    }
    routes = route orElse routes
  }

  def getImpl[A: c.WeakTypeTag](c: MContext)(path: c.Expr[String])(action: c.Expr[A])(writable: c.Expr[Writable[A]]): c.Expr[Unit] = {
    import c.universe._

    object ExtractMonad {
      val klass = rootMirror.staticPackage("org.http4s.scalatra").asModule.moduleClass.asType.toType
      val canUnwrapSymbol = klass.member(newTermName("canUnwrapImpl"))
      def unapply(tree: Tree): Option[Tree] = tree match {
        case Apply(TypeApply(canUnwrap, List(TypeTree())), List(monad)) if canUnwrap.symbol == canUnwrapSymbol =>
          Some(monad)
        case _ =>
          None
      }
    }

    def rewrite(tree: Tree) = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Apply(Apply(fun, args), List(ExtractMonad(monad))) =>
          val applied = Apply(monad, args)
          reify { unwrap(c.Expr[Action[_]](applied).splice)(Unapply.unapplyMA(implicitly[Monad[Action]])) }.tree

        case Apply(_, List(ExtractMonad(monad))) =>
          reify { unwrap(c.Expr[Action[_]](monad).splice)(Unapply.unapplyMA(implicitly[Monad[Action]])) }.tree

        case _ =>
          super.transform(tree)
      }
    }.transform(tree)
    val rewritten = c.resetLocalAttrs(rewrite(action.tree))

    reify {
      addRoute(path.splice, effectfully[Action, A] { c.Expr(rewritten).splice }, writable.splice)
    }
  }
}

trait ScalatraService extends HttpService {
  import ScalatraService._

  def apply(req: Request): Task[Response] = routes(req)

  def get[A](path: String)(action: A)(implicit writable: Writable[A]): Unit = macro getImpl[A]
  def getM[A](path: String)(action: => Action[A])(implicit writable: Writable[A]): Unit =
    addRoute(path, action, writable)

  def getResponse: State[Context, Response] = State.get[Context].map(_.res)
  def transformResponse(f: Response => Response): State[Context, Unit] =
    State.modify[Context](ctx => ctx.copy(res = f(ctx.res)))

  // Monadic API -- use in getM
  implicit def getStatus: Action[Status] = getResponse.map(_.status)
  implicit def setStatus: Status => Action[Unit] = { status => transformResponse(_.status(status)) }

  // Unwrapped API -- use in get
  def status(implicit canUnwrap: CanUnwrap[Action[Status]]): Status = ???
  def status_=(status: Status)(implicit canUnwrap: CanUnwrap[Status => Action[Unit]]): Unit = ???
}
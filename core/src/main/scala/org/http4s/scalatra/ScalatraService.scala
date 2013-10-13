package org.http4s.scalatra

import org.http4s._
import scalaz.concurrent.Task
import scala.reflect.macros.{Context => MContext}
import scalaz._
import effectful._
import scala.language.experimental.macros
import scalaz.State._
import scala.reflect.internal.annotations.compileTimeOnly
import scala.annotation.{TypeConstraint, StaticAnnotation}
import org.http4s.Request

object ScalatraService {
  type Action[A] = State[Context, A]

  case class Context(req: Request, params: Map[String, Seq[String]] = Map.empty, res: Response = Response())

  // FIXME Needs to be scoped to service
  var routes: PartialFunction[Request, Task[Response]] = {
    case req =>
      println(req.prelude.pathInfo)
      Status.NotFound()
  }

  def addRoute[A](path: String, action: Action[A], writable: Writable[A]) {
    val route: PartialFunction[Request, Task[Response]] = {
      case req: Request if req.prelude.pathInfo == path =>
        val (ctx, w) = action.run(Context(req))
        writable.toBody(w).map(body => ctx.res.copy(body = body._1))
    }
    routes = route orElse routes
  }

  def getImpl[A](c1: MContext)(path: c1.Expr[String])(action: c1.Expr[A])(writable: c1.Expr[Writable[A]]): c1.Expr[Unit] = {
    val effectfulAction: c1.Expr[Action[A]] = effectfullyImpl[Action, A](c1)(action)
    c1.universe.reify {
      addRoute(path.splice, effectfulAction.splice, writable.splice)
    }
  }

  def getResponse: State[Context, Response] = get[Context].map(_.res)
  def transformResponse(f: Response => Response): State[Context, Unit] =
    modify[Context](ctx => ctx.copy(res = f(ctx.res)))

  def getContentType: Action[Option[ContentType]] = getResponse.map(_.contentType)
  def setContentType(contentType: ContentType): Action[Unit] =
    transformResponse(_.contentType(contentType))

  def params: Action[Map[String, Seq[String]]] = get[Context].map(_.params)
  def param(name: String): Action[Option[String]] = params.map(_.get(name).flatMap(_.headOption))

  def getStatus: Action[Status] = getResponse.map(_.status)
  // TODO figure out how to make this implicit Unapply less explicit
  def statusImpl(c1: MContext): c1.Expr[Status] = {
    c1.universe.reify { unwrap(getStatus)(Unapply.unapplyMA(implicitly[Monad[ScalatraService.Action]])) }
  }

  def setStatus(status: Status): Action[Unit] = transformResponse(_.status(status))
  def setStatusImpl(c1: MContext)(status: c1.Expr[Status]): c1.Expr[Unit] = {
    c1.universe.reify { unwrap(setStatus(status.splice))(Unapply.unapplyMA(implicitly[Monad[ScalatraService.Action]])) }
  }
}

trait ScalatraService extends HttpService {
  import ScalatraService._

  def apply(req: Request): Task[Response] = routes(req)

  def GET[A](path: String)(action: A)(implicit writable: Writable[A]): Unit = macro getImpl[A]

  def status: Status = macro statusImpl
  def status_=(status: Status): Unit = macro setStatusImpl
}
package org.http4s.scalatra

import org.http4s._
import scalaz.concurrent.Task
import scala.reflect.macros.{Context => MContext}
import scalaz.State
import effectful._
import scala.language.experimental.macros
import scalaz.State._

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
    c1.universe.reify(addRoute(path.splice, effectfulAction.splice, writable.splice))
  }
}

trait ScalatraService extends HttpService {
  import ScalatraService._

  def apply(req: Request): Task[Response] = routes(req)

  def GET[A](path: String)(action: A)(implicit writable: Writable[A]): Unit = macro getImpl[A]

  /*
  */

  def response: State[Context, Response] = get[Context].map(_.res)
  def transformResponse(f: Response => Response): State[Context, Unit] =
    modify[Context](ctx => ctx.copy(res = f(ctx.res)))

  def contentType: Action[Option[ContentType]] = response.map(_.contentType)
  def contentType(contentType: ContentType): Action[Unit] =
    transformResponse(_.contentType(contentType))

  def status: Action[Status] = response.map(_.status)
  def status(status: Status): Action[Unit] =
    transformResponse(_.status(status))

  def params: Action[Map[String, Seq[String]]] = get[Context].map(_.params)
  def param(name: String): Action[Option[String]] = params.map(_.get(name).flatMap(_.headOption))
}
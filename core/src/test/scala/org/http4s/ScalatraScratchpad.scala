package org.http4s

import scalaz._
import Scalaz._
import effectful._
import scalaz.concurrent._

object ScalatraScratchpad extends App {
  trait Scalatra extends HttpService {
    private var routes: PartialFunction[Request, Task[Response]] = {
      case req =>
        println(req.prelude.pathInfo)
        Status.NotFound()
    }

    case class Context(req: Request, params: Map[String, Seq[String]] = Map.empty, res: Response = Response())

    def apply(req: Request): Task[Response] = routes(req)

    type Action[A] = StateT[Task, Context, A]
    implicit class ActionSyntax[A](self: => A) {
      def action: Action[A] = StateT[Task, Context, A](ctx => Task.delay((ctx, self)))
    }

    def GET[A: Writable](path: String)(action: Action[A]): Unit = routes = ({
      case req: Request if req.prelude.pathInfo == path =>
        action.run(Context(req)).flatMap { case (ctx, w) =>
          implicitly[Writable[A]].toBody(w).map(body => ctx.res.copy(body = body._1))
        }
    }: PartialFunction[Request, Task[Response]]) orElse routes

    def contentType: Action[Option[ContentType]] =
      StateT(ctx => Task.now((ctx, ctx.res.contentType)))
    def contentType(contentType: ContentType): Action[Unit] =
      StateT(ctx => Task.now((ctx.copy(res = ctx.res.contentType(contentType))), ()))

    def status: Action[Status] =
      StateT(ctx => Task.now((ctx, ctx.res.status)))
    def status(status: Status): Action[Unit] =
      StateT(ctx => Task.now((ctx.copy(res = ctx.res.status(status))), ()))

    def param(name: String): Action[Option[String]] =
      StateT(ctx => Task.now((ctx, ctx.params.get(name).flatMap(_.headOption))))
  }

  object Example extends Scalatra {
    GET("/baz") { for {
      _ <- status(Status.Ok)
      x <- param("x")
    } yield (s"x = ${x}") }

    GET("/baz/effectfully") { effectfully {
      status(Status.Ok).!
      val x = param("x").!
      s"x = ${x}"
    }}

    GET("/pure") { "pure".action }
  }

  println(Example(Request(RequestPrelude(pathInfo = "/pure"))).run)
}

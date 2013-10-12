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

    type Action[A] = State[Context, A]

    def GET[A: Writable](path: String)(action: Action[A]): Unit = routes = ({
      case req: Request if req.prelude.pathInfo == path =>
        action.run(Context(req)).flatMap { case (ctx, w) =>
          implicitly[Writable[A]].toBody(w).map(body => ctx.res.copy(body = body._1))
        }
    }: PartialFunction[Request, Task[Response]]) orElse routes

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

    GET("/pure") { state("/foo") }
  }

  println(Example(Request(RequestPrelude(pathInfo = "/pure"))).run)
}

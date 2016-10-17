package com.example.http4s.blaze

import java.util.concurrent._
import org.http4s._
import org.http4s.client.Client
import org.http4s.dsl._
import org.http4s.server._
import org.http4s.server.blaze._
import org.http4s.client.blaze._
import org.http4s.util.ProcessApp
import scalaz._
import scalaz.concurrent._
import scalaz.stream._, Process._

/** How to manage the lifecycle of a server and its dependent resources */
object NestedResourceCodensityExample extends ProcessApp {
  // This service depends on a client.
  def service(client: Client): HttpService = HttpService {
    case GET -> Root / "proxy" =>
      client.toHttpService.run(Request(Method.GET, uri("http://http4s.org/")))
  }

  trait Releaseable[A] {
    def release(a: A): Process[Task, Nothing]
  }

  object Releaseable {
    def withRelease[A](f: A => Process[Task, Nothing]): Releaseable[A] =
      new Releaseable[A] { def release(a: A) = f(a) }

    implicit val ExecutorServiceReleaseable: Releaseable[ExecutorService] =
      withRelease(a => eval_(Task.delay(a.shutdown())))

    implicit val ServerReleaseable: Releaseable[Server] =
      withRelease(a => eval_(a.shutdown))

    implicit val ClientReleaseable: Releaseable[Client] =
      withRelease(a => eval_(a.shutdown))
  }

  type Resource[A] = Codensity[({type l[x]=Process[Task,x]})#l, A]

  object Resource {
    def bracket[A](acquire: Task[A])(release: A => Process[Task, Nothing]): Resource[A] =
      new Resource[A] {
        def apply[B](f: A => Process[Task, B]): Process[Task, B] =
          Process.bracket(acquire)(release)(f)
      }

    def apply[A](a: Task[A])(implicit r: Releaseable[A]): Resource[A] =
      bracket(a)(r.release)

    def delay[A](a: => A)(implicit r: Releaseable[A]): Resource[A] =
      apply(Task.delay(a))

    val awaitShutdown: Resource[Unit] =
      bracket(Task.async[Unit] { _ => })(_ => halt)
  }

  def main(args: List[String]) =
    (for {
      clientExecutor <- Resource.delay(Executors.newFixedThreadPool(10))
      client         <- Resource.delay(PooledHttp1Client(config = BlazeClientConfig.defaultConfig.copy(customExecutor = Some(clientExecutor))))
      serverExecutor <- Resource.delay(Executors.newFixedThreadPool(10))
      server         <- Resource(BlazeBuilder.withServiceExecutor(serverExecutor).mountService(service(client)).start)
      _              <- Resource.awaitShutdown
    } yield ()).improve
}

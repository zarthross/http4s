package org.http4s.server

import java.util.concurrent.ExecutorService

import scala.concurrent.duration.Duration
import scalaz.concurrent.Strategy
import scalaz.{Free, Functor}

sealed trait ServerConfig[A]

object ServerConfig {
  case class SetPort[A](port: Int, next: A) extends ServerConfig[A]
  case class SetHost[A](host: String, next: A) extends ServerConfig[A]
  case class SetNio2[A](isNio2: Boolean, next: A) extends ServerConfig[A]
  case class SetIdleTimeout[A](duration: Duration, next: A) extends ServerConfig[A]
  case class SetConnectionTimeout[A](duration: Duration, next: A) extends ServerConfig[A]
  case class SetAsyncTimeout[A](duration: Duration, next: A) extends ServerConfig[A]

  case class MountService[A](service: HttpService, prefix: String, executor: ExecutorService, next: A) extends ServerConfig[A]

  implicit def serverConfigFunctor: Functor[ServerConfig] = new Functor[ServerConfig] {
    def map[A, B](fa: ServerConfig[A])(f: A => B): ServerConfig[B] = fa match {
      case SetPort(port, next) => SetPort(port, f(next))
      case SetHost(host, next) => SetHost(host, f(next))
      case SetNio2(isNio2, next) => SetNio2(isNio2, f(next))
      case SetIdleTimeout(duration, next: A) => SetIdleTimeout(duration, f(next))
      case SetConnectionTimeout(duration, next) => SetConnectionTimeout(duration, f(next))
      case SetAsyncTimeout(duration, next) => SetAsyncTimeout(duration, f(next))
      case MountService(service, prefix, executor, next) => MountService(service, prefix, executor, f(next))
    }
  }

  def setPort(port: Int) = Free.liftF[ServerConfig, Unit](SetPort(port, ()))

  def setHost(host: String) = Free.liftF[ServerConfig, Unit](SetHost(host, ()))

  def setNio2(isNio2: Boolean) = Free.liftF[ServerConfig, Unit](SetNio2(isNio2, ()))

  def setIdleTimeout(duration: Duration) = Free.liftF[ServerConfig, Unit](SetIdleTimeout(duration, ()))

  def setConnectionTimeout(duration: Duration) = Free.liftF[ServerConfig, Unit](SetConnectionTimeout(duration, ()))

  def setAsyncTimeout(duration: Duration) = Free.liftF[ServerConfig, Unit](SetAsyncTimeout(duration, ()))

  def mountService(service: HttpService, prefix: String = "")(implicit executor: ExecutorService = Strategy.DefaultExecutorService) =
    Free.liftF[ServerConfig, Unit](MountService(service, prefix, executor, ()))
}

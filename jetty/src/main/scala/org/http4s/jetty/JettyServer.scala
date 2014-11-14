package org.http4s
package jetty

import org.eclipse.jetty.server.{Server => JServer, ServerConnector}
import org.eclipse.jetty.servlet.{ServletHolder, ServletContextHandler}
import org.http4s.server.{Server, ServerConfig}
import org.http4s.server.ServerConfig._
import org.log4s.getLogger
import scala.concurrent.duration.Duration
import scalaz.Free
import scalaz.concurrent.Task
import org.http4s.servlet.Http4sServlet
import org.eclipse.jetty.util.component.AbstractLifeCycle.AbstractLifeCycleListener
import org.eclipse.jetty.util.component.LifeCycle
import org.log4s.getLogger

class JettyServer private[jetty] (server: JServer) extends Server {
  private[this] val logger = getLogger

  def start: Task[this.type] = Task.delay {
    server.start()
    this
  }

  def shutdown: Task[this.type] = Task.delay {
    server.stop()
    this
  }

  def join(): this.type = {
    server.join()
    this
  }

  override def onShutdown(f: => Unit): this.type = {
    server.addLifeCycleListener { new AbstractLifeCycleListener {
      override def lifeCycleStopped(event: LifeCycle): Unit = f
    }}
    this
  }
}

object JettyServer {
  def apply(config: Free[ServerConfig, Unit]): JettyServer = {
    val server = new JServer()
    var port = 8080
    var host = "0.0.0.0"
    var idleTimeout: Duration = Duration.Inf
    var asyncTimeout: Duration = Duration.Inf
    var mounts: Vector[MountService[_]] = Vector.empty

    config.go {
      case SetPort(p, next) =>
        port = p
        next
      case SetHost(h, next) =>
        host = h
        next
      case SetNio2(useNio2, next) =>
        // noop
        next
      case SetIdleTimeout(timeout, next) =>
        idleTimeout = timeout
        next
      case SetAsyncTimeout(timeout, next) =>
        asyncTimeout = timeout
        next
      case SetConnectionTimeout(timeout, next) =>
        // noop
        next
      case mount @ MountService(service, prefix, executor, next) =>
        mounts :+= mount
        next
    }

    val context = new ServletContextHandler()
    context.setContextPath("/")
    server.setHandler(context)

    val connector = new ServerConnector(server)
    connector.setHost(host)
    connector.setPort(port)
    connector.setIdleTimeout(if (idleTimeout.isFinite) idleTimeout.toMillis else -1)

    for ((mount, i) <- mounts.zipWithIndex) {
      val servlet = new Http4sServlet(mount.service, asyncTimeout = asyncTimeout, threadPool = mount.executor)
      val servletName = s"http4s-servlet-$i"
      context.addServlet(new ServletHolder(servletName, servlet), s"${mount.prefix}/*")
    }

    server.addConnector(connector)
    new JettyServer(server)
  }
}

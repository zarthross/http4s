package org.http4s
package tomcat

import javax.servlet.http.HttpServlet
import org.apache.catalina.connector.Connector
import org.apache.coyote.http11.{Http11NioProtocol, Http11Nio2Protocol}
import org.http4s.server.{Server, ServerConfig}
import org.http4s.server.ServerConfig._
import scala.concurrent.duration.Duration
import scalaz.Free
import scalaz.concurrent.Task
import org.apache.catalina.startup.Tomcat
import org.http4s.servlet.Http4sServlet
import org.apache.catalina.{LifecycleState, Lifecycle, LifecycleEvent, LifecycleListener}
import java.util.concurrent.CountDownLatch
import org.log4s.getLogger

class TomcatServer private[tomcat] (tomcat: Tomcat) extends Server {
  def start: Task[this.type] = Task.delay {
    tomcat.start()
    this
  }

  def shutdown: Task[this.type] = Task.delay {
    tomcat.stop()
    this
  }

  def join(): this.type = {
    if (tomcat.getServer.getState.ordinal < LifecycleState.STOPPED.ordinal) {
      val latch = new CountDownLatch(1)
      onShutdown { latch.countDown() }
      latch.await()
    }
    this
  }

  override def onShutdown(f: => Unit): this.type = {
    tomcat.getServer.addLifecycleListener(new LifecycleListener {
      override def lifecycleEvent(event: LifecycleEvent): Unit = {
        if (Lifecycle.AFTER_STOP_EVENT.equals(event.getLifecycle))
          f
      }
    })
    this
  }
}

object TomcatServer {
  private[this] val logger = getLogger

  def apply(config: Free[ServerConfig, Unit]): TomcatServer = {
    var port: Int = 8080
    var host: String = "0.0.0.0"
    var idleTimeout: Duration = Duration.Inf
    var asyncTimeout: Duration = Duration.Inf
    var connectionTimeout: Duration = Duration.Inf
    var isNio2: Boolean = true
    var mounts: Vector[MountService[_]] = Vector.empty

    val tomcat = new Tomcat

    tomcat.addContext("", getClass.getResource("/").getPath)

    config.go {
      case SetPort(p, next) =>
        port = p
        next
      case SetHost(h, next) =>
        host = h
        next
      case SetNio2(b, next) =>
        isNio2 = b
        next
      case SetIdleTimeout(timeout, next) =>
        idleTimeout = timeout
        next
      case SetAsyncTimeout(timeout, next) =>
        asyncTimeout = timeout
        next
      case SetConnectionTimeout(timeout, next) =>
        connectionTimeout = timeout
        next
      case mount @ MountService(service, prefix, executor, next) =>
        mounts :+= mount
        next
    }

    val connector = new Connector(
      if (isNio2)
        classOf[Http11Nio2Protocol].getName
      else
        classOf[Http11NioProtocol].getName
    )
    connector.setAttribute("address", host)
    connector.setAttribute("connection_pool_timeout", if (idleTimeout.isFinite) idleTimeout.toSeconds.toInt else 0)
    connector.setAttribute("connectionTimeout", if (connectionTimeout.isFinite) connectionTimeout.toMillis.toInt else -1)
    connector.setPort(port)
    tomcat.getService.addConnector(connector)
    tomcat.setConnector(connector)

    for ((mount, i) <- mounts.zipWithIndex) {
      val servlet = new Http4sServlet(mount.service, asyncTimeout = asyncTimeout, threadPool = mount.executor)
      val wrapper = tomcat.addServlet("", s"http4s-servlet-$i", servlet)
      wrapper.addMapping(s"${mount.prefix}/*")
      wrapper.setAsyncSupported(true)
    }

    new TomcatServer(tomcat)
  }
}

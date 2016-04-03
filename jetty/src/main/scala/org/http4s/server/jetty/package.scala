package org.http4s
package server

package object jetty {

  import java.net.InetSocketAddress
  import org.eclipse.jetty.server.{ NetworkConnector, ServerConnector }
  import org.eclipse.jetty.util.component.AbstractLifeCycle.AbstractLifeCycleListener
  import org.eclipse.jetty.util.component.LifeCycle
  import org.http4s.util.tap._
  import scalaz.concurrent.Task

  private[this] val logger = org.log4s.getLogger

  val Jetty = JettyConfig { server =>
    Task.delay {
      server.start()

      val myAddress = 
        server.getConnectors.collectFirst {
          case connector: NetworkConnector =>
            val host = Option(connector.getHost).getOrElse("0.0.0.0")
            val port = connector.getLocalPort
            new InetSocketAddress(host, port)
        }.getOrElse(new InetSocketAddress("0.0.0.0", 0))
      logger.info(s"Jetty server started on ${myAddress}")

      new Server {
        override def shutdown: Task[Unit] =
          Task.delay {
            server.stop()
          }

        override def onShutdown(f: => Unit): this.type = {
          server.addLifeCycleListener {
            new AbstractLifeCycleListener {
              override def lifeCycleStopped(event: LifeCycle): Unit = f
            }}
          this
        }

        val address: InetSocketAddress =
          myAddress
      }
    }
  }
}

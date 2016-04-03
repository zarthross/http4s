package org.http4s
package server

package object jetty {

  import org.eclipse.jetty.server.ConnectionFactory
  import org.eclipse.jetty.servlet.ServletContextHandler
  import scala.collection.JavaConverters._
  import com.codahale.metrics.MetricRegistry
  import com.codahale.metrics.jetty9.InstrumentedQueuedThreadPool
  import java.net.InetSocketAddress
  import org.eclipse.jetty.server.{ AbstractConnector, Connector, NetworkConnector, Server => JettyServer, ServerConnector }
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

  val Default: JettyServer =
    new JettyServer()
      .tap(_.setHandler(
         new ServletContextHandler()
           .tap(_.setContextPath("/"))
      ))
      .tap(jetty => jetty.addConnector(
         new ServerConnector(jetty)
      ))

  def instrumented(metricRegistry: MetricRegistry,
                        prefix: String = "org.http4s.server"): JettyServer =
      new JettyServer(new InstrumentedQueuedThreadPool(metricRegistry)) {
        override def addConnector(connector: Connector): Unit =
          super.addConnector(instrumentConnector(connector))

        override def setConnectors(connectors: Array[Connector]): Unit =
          super.setConnectors(connectors.map(instrumentConnector(_)))

        private def instrumentConnector(connector: Connector): connector.type = 
          connector match {
            case c: AbstractConnector =>
              c.setConnectionFactories(
                connector.getConnectionFactories.asScala.map { factory =>
                  val name = MetricRegistry.name(prefix, "connections", factory.getProtocol)
                  val timer = metricRegistry.timer(name)
                  new InstrumentedConnectionFactory(factory, timer): ConnectionFactory
                }.asJavaCollection
              )
              connector
            case other =>
              connector
          }
      }
      .tap(_.setHandler(
        new InstrumentedHandler(metricRegistry, Some(prefix))
          .tap(_.setHandler(
            new ServletContextHandler()
              .tap(_.setContextPath("/"))
          ))
      ))
      .tap(jetty => jetty.addConnector(
         new ServerConnector(jetty)
           .tap(_.setPort(8080))
      ))    
}

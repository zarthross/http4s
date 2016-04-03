package org.http4s
package server
package jetty

import com.codahale.metrics.MetricRegistry
import com.codahale.metrics.jetty9.InstrumentedQueuedThreadPool
import java.util.{ EnumSet, UUID }
import javax.servlet.{ DispatcherType, Filter }
import javax.servlet.http.HttpServlet
import org.eclipse.jetty.server.handler.{ HandlerCollection, HandlerWrapper }
import org.eclipse.jetty.util.thread.ThreadPool
import scala.annotation.tailrec

import scala.collection.JavaConverters._
import org.eclipse.jetty.server.{Server => JettyServer, Request => _, Response => _, _}
import org.eclipse.jetty.servlet.{ FilterHolder, ServletContextHandler, ServletHolder }
import org.eclipse.jetty.util.ssl.SslContextFactory
import org.eclipse.jetty.util.thread.QueuedThreadPool
import org.http4s.server.SSLSupport.StoreInfo
import org.http4s.servlet.Http4sServlet
import org.http4s.util.tap._
import scalaz.ReaderT
import scalaz.concurrent.Task

class JettyConfig private[jetty] (config: ReaderT[Task, JettyServer, Server]) {
  private[this] val logger = org.log4s.getLogger

  def start(jetty: JettyServer = Default): Task[Server] =
    config.run(jetty)

  def run(jetty: JettyServer = Default): Server =
    start(jetty).run

  def configure(f: JettyServer => Unit): JettyConfig =
    JettyConfig(config.local { jetty: JettyServer => f(jetty); jetty })

  /** Adds an HTTP connector.
   *  By default, listens to port 8080 on any address.
   */
  def bindHttp(port: Int = 8080, host: String = "0.0.0.0"): JettyConfig =
    configure { jetty =>
      jetty.addConnector {
        new ServerConnector(jetty)
          .tap(_.setPort(port))
          .tap(_.setHost(host))
      }
    }

  /** Adds an HTTPS connector.
   *  By default, listens to port 8443 on any address.
   */
  def bindHttps(keyStore: StoreInfo,
                keyManagerPassword: String,
                protocol: String = "TLS",
                trustStore: Option[StoreInfo] = None,
                clientAuth: Boolean = false,
                port: Int = 8443,
                host: String = "127.0.0.1"): JettyConfig =
    configure { jetty =>
      jetty.addConnector {
        new ServerConnector(
          jetty,
          new SslConnectionFactory(
            new SslContextFactory()
              .tap(_.setKeyStorePath(keyStore.path))
              .tap(_.setKeyStorePassword(keyStore.password))
              .tap(_.setNeedClientAuth(clientAuth))
              .tap(_.setProtocol(protocol))
              .tap { factory =>
              trustStore.foreach { ts =>
                factory.setTrustStorePath(ts.path)
                factory.setTrustStorePassword(ts.password)
              }
            },
            org.eclipse.jetty.http.HttpVersion.HTTP_1_1.asString),
          new HttpConnectionFactory(
            new HttpConfiguration()
              .tap(_.setSecureScheme("https"))
              .tap(_.setSecurePort(port))
              .tap(_.addCustomizer(new SecureRequestCustomizer))))
      }
    }

  def apply(f: Request => Task[Response]): JettyConfig =
    mountService(HttpService.lift(f), "/")

  def mountService(service: HttpService, prefix: String = "/") =
    mountServlet(new Http4sServlet(service),
                 prefix match {
                   case p if p endsWith "/*" => p
                   case p => s"$p/*"
                 })

  private def configureServletContextHandler(f: ServletContextHandler => Unit): JettyConfig =
    configure { jetty =>
      def loop(h: Handler): Option[ServletContextHandler] =
        h match {
          case h: ServletContextHandler => Some(h)
          case h: HandlerWrapper => h.getHandlers.view.map(loop).collectFirst {
            case Some(child) => child
          }
          case _ => None
        }
      loop(jetty) match {
        case None => logger.error("Could not find ServletContextHandler")
        case Some(handler) => f(handler)
      }
    }


  def mountServlet(servlet: HttpServlet, prefix: String = "/*") =
    configureServletContextHandler(
      _.addServlet(
        new ServletHolder(
          UUID.randomUUID.toString,
          servlet),
        prefix)
    )

  def mountFilter(filter: Filter,
                  urlMapping: String = "/*",
                  dispatches: EnumSet[DispatcherType] = EnumSet.of(
                    DispatcherType.REQUEST,
                    DispatcherType.FORWARD,
                    DispatcherType.INCLUDE,
                    DispatcherType.ASYNC))
                  : JettyConfig =
    configureServletContextHandler(
      _.addFilter(
        new FilterHolder(filter)
          .tap(_.setName(UUID.randomUUID.toString)),
        urlMapping,
        dispatches)
    )
}

object JettyConfig {
  def apply(run: ReaderT[Task, JettyServer, Server]): JettyConfig = new JettyConfig(run)
  def apply(f: JettyServer => Task[Server]): JettyConfig = new JettyConfig(ReaderT(f))
}


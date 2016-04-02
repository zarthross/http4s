package org.http4s
package server
package jetty

import java.util.UUID

import org.eclipse.jetty.server.{Server => JettyServer, Request => _, Response => _, _}
import org.eclipse.jetty.servlet.{ServletContextHandler, ServletHolder}
import org.eclipse.jetty.util.ssl.SslContextFactory
import org.http4s.server.SSLSupport.StoreInfo
import org.http4s.servlet.Http4sServlet
import org.http4s.util.tap._
import scalaz.ReaderT
import scalaz.concurrent.Task

class JettyConfig private[jetty] (config: ReaderT[Task, JettyServer, Server]) {
  /**
   * Starts a server.  If no connector has been added, an HTTP
   * connector will be started on 0.0.0.0:8080.
   */
  def start: Task[Server] =
    config.run {
      new JettyServer()
        .tap(_.setHandler(
               new ServletContextHandler()
                 .tap(_.setContextPath("/"))
             ))
    }

  def run: Server =
    start.run

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
    configure { jetty =>
      jetty.getHandlers.collectFirst {
        case handler: ServletContextHandler =>
          handler.addServlet(
            new ServletHolder(
              UUID.randomUUID.toString,
              new Http4sServlet(service)),
            s"$prefix/*")
      }
    }
}

object JettyConfig {
  def apply(run: ReaderT[Task, JettyServer, Server]): JettyConfig = new JettyConfig(run)
  def apply(f: JettyServer => Task[Server]): JettyConfig = new JettyConfig(ReaderT(f))
}


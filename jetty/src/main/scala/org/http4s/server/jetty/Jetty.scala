package org.http4s
package server

package object jetty {
  import java.net.InetSocketAddress
  import java.util.UUID
  import org.eclipse.jetty.server.{ Server => JettyServer, Request => _, Response => _, _ }
  import org.eclipse.jetty.servlet.{ ServletContextHandler, ServletHolder }
  import org.eclipse.jetty.util.component.AbstractLifeCycle.AbstractLifeCycleListener
  import org.eclipse.jetty.util.component.LifeCycle
  import org.eclipse.jetty.util.ssl.SslContextFactory
  import org.http4s.server.SSLSupport.StoreInfo
  import org.http4s.servlet.Http4sServlet
  import scalaz.ReaderT
  import org.eclipse.jetty.server.{ Server => JettyServer }
  import scalaz.concurrent.Task

  class JettyConfig private (config: ReaderT[Task, JettyServer, Server]) {
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

    def http(port: Int = 8080, host: String = "127.0.0.1"): JettyConfig =
      configure { jetty =>
        jetty.addConnector {
          new ServerConnector(jetty)
            .tap(_.setPort(port))
            .tap(_.setHost(host))
        }
      }

    def https(keyStore: StoreInfo,
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
      mount(HttpService.lift(f))

    def mount(service: HttpService) =
      configure { jetty =>
        jetty.getHandlers.collectFirst {
          case handler: ServletContextHandler =>
            handler.addServlet(
              new ServletHolder(
                UUID.randomUUID.toString,
                new Http4sServlet(service)),
              "/*")
          }
      }
  }


  object JettyConfig {
    def apply(run: ReaderT[Task, JettyServer, Server]): JettyConfig = new JettyConfig(run)
    def apply(f: JettyServer => Task[Server]): JettyConfig = new JettyConfig(ReaderT(f))
  }


  val Jetty: JettyConfig = JettyConfig { jetty =>
    Task.delay {
      jetty.start()

      new Server {
        override def shutdown: Task[Unit] =
          Task.delay {
            jetty.stop()
          }

        override def onShutdown(f: => Unit): this.type = {
          jetty.addLifeCycleListener { new AbstractLifeCycleListener {
            override def lifeCycleStopped(event: LifeCycle): Unit = f
          }}
          this
        }

        lazy val address: InetSocketAddress = {
          jetty.getConnectors().collectFirst {
            case connector: ServerConnector =>
              val host = connector.getHost
              val port = connector.getLocalPort
              new InetSocketAddress(host, port)
          }.getOrElse(new InetSocketAddress("0.0.0.0", 0))
        }
      }
    }
  }

  private implicit class TapSyntax[A <: AnyRef](val self: A) extends AnyVal {
    final def tap(f: A => Unit): self.type = {
      f(self); self
    }
  }

  implicit class JettyConfigOps(self: JettyConfig) {
  }
}


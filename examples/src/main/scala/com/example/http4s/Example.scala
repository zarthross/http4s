package com.example.http4s

import org.http4s.jetty.JettyServer
import org.http4s.server.ServerConfig._
import org.http4s.server.blaze.BlazeServer
import org.http4s.tomcat.TomcatServer

object ExampleConfig {
  lazy val config = {
    for {
      _ <- setPort(8080)
      _ <- setHost("localhost")
      _ <- setNio2(true)
      _ <- mountService(ExampleService.service, "/http4s")
    } yield ()
  }
}
import ExampleConfig.config

object BlazeExample extends App {
  BlazeServer(config).start.run
}

object JettyExample extends App {
  JettyServer(config).run().join()
}

object TomcatExample extends App {
  TomcatServer(config).run().join()
}


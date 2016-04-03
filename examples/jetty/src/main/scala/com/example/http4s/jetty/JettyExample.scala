package com.example.http4s
package jetty

import javax.servlet._

import com.codahale.metrics.MetricRegistry
import com.codahale.metrics.servlets.MetricsServlet
import org.http4s.server.jetty.Jetty

object JettyExample extends App {
  val metrics = new MetricRegistry

  Jetty
    .mountService(ExampleService.service, "/http4s")
    .mountServlet(new MetricsServlet(metrics), "/metrics/*")
    .mountFilter(NoneShallPass, "/http4s/science/black-knight/*")
    .startInstrumented(metrics)
    .run
}

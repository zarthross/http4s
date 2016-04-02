package org.http4s
package server
package jetty

import org.eclipse.jetty.server.Server

object Example extends App {
  Jetty.http(8081) {
    case req => Response(Status.Ok).withBody("Hello, world.")
  }.run
}

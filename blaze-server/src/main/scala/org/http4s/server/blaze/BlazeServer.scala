package org.http4s
package server
package blaze

import java.util.concurrent.ExecutorService

import org.http4s.blaze.pipeline.LeafBuilder
import org.http4s.blaze.pipeline.stages.QuietTimeoutStage
import org.http4s.blaze.channel.{SocketConnection, ServerChannel}
import org.http4s.blaze.channel.nio1.SocketServerChannelFactory
import org.http4s.blaze.channel.nio2.NIO2ServerChannelFactory
import org.http4s.server.ServerConfig._

import server.middleware.URITranslation

import java.net.InetSocketAddress
import scala.concurrent.duration.Duration
import java.nio.ByteBuffer

import scalaz.Free
import scalaz.concurrent.{Strategy, Task}

class BlazeServer private (serverChannel: ServerChannel) extends Server {
  override def start: Task[this.type] = Task.delay {
    serverChannel.run()
    this
  }

  override def shutdown: Task[this.type] = Task.delay {
    serverChannel.close()
    this
  }

  override def onShutdown(f: => Unit): this.type = {
    serverChannel.addShutdownHook(() => f)
    this
  }
}

object BlazeServer {
  def apply(config: Free[ServerConfig, Unit]): BlazeServer = {
    var aggregateService = Service.empty[Request, Response]
    var port = 8080
    var idleTimeout: Duration = Duration.Inf
    var host = "0.0.0.0"
    var isnio2 = false
    var threadPool: ExecutorService = Strategy.DefaultExecutorService

    config.go {
      case SetPort(p, next) =>
        port = p
        next
      case SetHost(h, next) =>
        host = h
        next
      case SetNio2(b, next) =>
        isnio2 = b
        next
      case SetIdleTimeout(timeout, next) =>
        idleTimeout = timeout
        next
      case SetAsyncTimeout(timeout, next) =>
        // noop
        next
      case SetConnectionTimeout(timeout, next) =>
        // noop
        next
      case MountService(service, prefix, executor, next) =>
        val prefixedService =
          if (prefix.isEmpty || prefix == "/") service
          else URITranslation.translateRoot(prefix)(service)
        aggregateService =
          if (aggregateService.run eq Service.empty.run) prefixedService
          else prefixedService orElse aggregateService
        next
    }

    def pipelineFactory(conn: SocketConnection): LeafBuilder[ByteBuffer] = {
      val leaf = LeafBuilder(new Http1ServerStage(aggregateService, Some(conn), threadPool))
      if (idleTimeout.isFinite) leaf.prepend(new QuietTimeoutStage[ByteBuffer](idleTimeout))
      else leaf
    }

    val factory = if (isnio2) new NIO2ServerChannelFactory(pipelineFactory)
    else new SocketServerChannelFactory(pipelineFactory, 12, 8 * 1024)

    val address = new InetSocketAddress(host, port)
    if (address.isUnresolved) throw new Exception(s"Unresolved hostname: $host")

    val channel = factory.bind(address)
    new BlazeServer(channel)
  }
}

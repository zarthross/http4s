package org.http4s
package server

import java.util.concurrent.ExecutorService

import scalaz.concurrent.Task
import scala.concurrent.duration.Duration

trait Server {
  def start: Task[this.type]

  def run(): this.type = start.run

  def shutdown: Task[this.type]

  def shutdownNow(): this.type = shutdown.run

  def onShutdown(f: => Unit): this.type
}

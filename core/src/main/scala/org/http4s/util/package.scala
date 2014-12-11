package org.http4s

import java.util.{List => JList, Collections}
import java.util.concurrent.{TimeUnit, AbstractExecutorService}

import scala.concurrent.{ExecutionContextExecutorService, ExecutionContext}

package object util {
  def executionContextToExecutorService(ec: ExecutionContext): ExecutionContextExecutorService = ec match {
    case es: ExecutionContextExecutorService => es
    case ec => new AbstractExecutorService with ExecutionContextExecutorService {
      override def execute(runnable: Runnable): Unit = ec.execute(runnable)
      override def reportFailure(t: Throwable): Unit = ec.reportFailure(t)
      override def shutdown(): Unit = {}
      override def isTerminated: Boolean = false
      override def awaitTermination(timeout: Long, unit: TimeUnit): Boolean = false
      override def shutdownNow(): JList[Runnable] = Collections.emptyList[Runnable]
      override def isShutdown: Boolean = false
      override def prepare(): ExecutionContext = ec
    }
  }
}

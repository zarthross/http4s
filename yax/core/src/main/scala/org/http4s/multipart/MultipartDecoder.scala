package org.http4s
package multipart

import scala.util.{Try, Success, Failure}
import scala.util.control._
import org.http4s._
import parser._
import headers._
import Http4s._
import org.http4s.util._

import scalaz._
import Scalaz._
import scodec.bits.ByteVector
import org.parboiled2._
import org.log4s.getLogger

#+scalaz-stream
import scalaz.stream.Process
import scalaz.concurrent.Task
#-scalaz-stream
#+fs2
import fs2.{Stream => Process, Task}
#-fs2

private[http4s] object MultipartDecoder {
  import Process._

  private[this] val logger = getLogger
  
  val decoder: EntityDecoder[Multipart] =
    EntityDecoder.decodeBy(MediaRange.`multipart/*`) { msg =>
      def gatherParts = {
        def go(part: Part): Process1[Headers \/ ByteVector, Part] =
          receive1Or[Headers \/ ByteVector, Part](emit(part)) {
            case -\/(headers) =>
              emit(part) fby go(Part(headers, EmptyBody))
            case \/-(chunk) =>
              go(part.copy(body = part.body ++ emit(chunk)))
          }

        receive1[Headers \/ ByteVector, Part] {
          case -\/(headers) =>
            go(Part(headers, EmptyBody))
          case \/-(chunk) =>
            Process.fail(InvalidMessageBodyFailure("No headers in first part"))
        }
      }

      msg.contentType.flatMap(_.mediaType.extensions.get("boundary")) match {
        case Some(boundary) =>
          DecodeResult {
            msg.body
#+scalaz-stream
              .pipe(MultipartParser.parse(Boundary(boundary)))
              .pipe(gatherParts)
#-scalaz-stream
#+fs2
              .through(MultipartParser.parse(Boundary(boundary)))
              .through(gatherParts)
#-fs2
              .runLog
              .map(parts => \/-(Multipart(parts, Boundary(boundary))))
              .handle {
                case e: InvalidMessageBodyFailure => -\/(e)
                case e => -\/(InvalidMessageBodyFailure("Invalid multipart body", Some(e)))
            }
          }
        case None =>
          DecodeResult.failure(InvalidMessageBodyFailure("Missing boundary extension to Content-Type"))
      }
    }
}

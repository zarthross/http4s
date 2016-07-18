package org.http4s
package multipart

import scala.util.Random

import org.http4s._
import org.http4s.MediaType._
import org.http4s.headers.{ `Content-Type` ⇒ ContentType, `Content-Disposition` => ContentDisposition }
import org.http4s.Http4s._
import org.http4s.Uri._
import org.http4s.util._
import org.http4s.EntityEncoder._
import Entity._
import scodec.bits.ByteVector

#+scalaz-stream
import scalaz.concurrent.Task
import scalaz.stream.Process
#-scalaz-stream
#+fs2
import fs2.{Chunk, Stream => Process, Task}
#-fs2

private[http4s] object MultipartEncoder extends EntityEncoder[Multipart] {

  //TODO: Refactor encoders to create headers dependent on value.
  def headers: Headers = Headers.empty

  def toEntity(mp: Multipart): Task[Entity] = {
    val dash             = "--"
    val dashBoundary:  Boundary => String =     boundary =>
                       new StringWriter()                <<
                       dash                              << 
                       boundary.value              result()    
    val delimiter:     Boundary => String =     boundary =>
                       new StringWriter()                <<
                       Boundary.CRLF                     <<
                       dash                              << 
                       boundary.value              result()
    val closeDelimiter:Boundary => String =     boundary =>
                       new StringWriter()                <<
                       delimiter(boundary)               <<
                       dash                        result()          
    val start:         Boundary => ByteVector = boundary =>
                       ByteVectorWriter()                <<
                       dashBoundary(boundary)            <<
                       Boundary.CRLF         toByteVector()
    val end:           Boundary => ByteVector = boundary =>
                       ByteVectorWriter()                <<
                       closeDelimiter(boundary) toByteVector()
    val encapsulation: Boundary => String =     boundary =>
                       new StringWriter()                <<
                       Boundary.CRLF                     <<
                       dashBoundary(boundary)            <<
                       Boundary.CRLF               result()    

    val _start         = start(mp.boundary)
#+scalaz-stream    
    val _end           = Process.emit(end(mp.boundary))
#-scalaz-stream
#+fs2
    val _end           = Process.chunk(Chunk.bytes(end(mp.boundary).toArray))
#-fs2
    val _encapsulation = ByteVector(encapsulation(mp.boundary).getBytes)

    def renderPart(prelude: ByteVector, p: Part): EntityBody = {
      val header = Process.emit(prelude ++ (p.headers.foldLeft(ByteVectorWriter()) { (w, h) =>
        w << h << Boundary.CRLF
      } << Boundary.CRLF).toByteVector)
#+scalaz-stream
      header ++ p.body
#-scalaz-stream
#+fs2
      // TODO is there a better way here?
      header.map(bv => Chunk.bytes(bv.toArray)).flatMap(Process.chunk) ++ p.body
#-fs2
    }

    val parts = mp.parts
    val body = parts.tail.foldLeft(renderPart(_start, parts.head)) { (acc, part) =>
      acc ++ renderPart(_encapsulation, part)
    } ++ _end

    Task.now(EntityEncoder.Entity(body, None))
  }
}

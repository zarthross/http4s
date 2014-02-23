package org.http4s.scalatra

import org.scalatest.{Matchers, WordSpec}
import org.http4s.Request

/**
 * @author Bryce Anderson
 *         Created on 2/22/14
 */
class ScalatraServiceSpec extends WordSpec with Matchers {

  val srvc = new ScalatraService {

    get("/foo"){

      val a = request.queryString

      "Hello world!"
    }

    // Getting it wrong
//    val b = request
  }

  "ScalatraService" should {
    "Say hello" in {
      "Hello"
    }
  }

}

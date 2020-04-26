package com.nat.photosynthesis.controller

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.testkit.ScalatestRouteTest
import cats.effect.IO
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FreeSpec, Matchers}

class CustomDirectivesSpec
  extends FreeSpec
    with Matchers
    with ScalatestRouteTest
    with MockitoSugar {

  import CustomDirectives._

  "onIOComplete" - {
    "should use the same way as onComplete(future...)" in {
      val ioResult = IO { "something" }

      Get("/") ~> onIOComplete(ioResult) {
        case scala.util.Success(a) => complete(a)
        case scala.util.Failure(_) => complete("error")
      } ~> check {
        status shouldBe StatusCodes.OK
        responseAs[String] shouldBe "something"
      }
    }

    "should fail when io has exception" in {
      val ioResult = IO[String] { throw new Exception("error") }

      Get("/") ~> onIOComplete(ioResult) {
        case scala.util.Success(a) => complete(a)
        case scala.util.Failure(_) => complete("error")
      } ~> check {
        status shouldBe StatusCodes.OK
        responseAs[String] shouldBe "error"
      }
    }
  }
}

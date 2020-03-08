package com.nat.photosynthesis.controller

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{FreeSpec, Matchers}

class GameRouteSpec extends FreeSpec with Matchers with ScalatestRouteTest {
  val gameRoute = new GameRoute()

  "/" - {
    "GET" - {
      "should returns empty array if there is no active games" in {
        Get() ~> gameRoute.route ~> check {
          responseAs[String] shouldEqual "[]"
        }
      }
    }
  }
}

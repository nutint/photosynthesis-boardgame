package com.nat.photosynthesis.controller

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{FreeSpec, Matchers}
import akka.http.scaladsl.server.Directives._

class MainRouteSpec extends FreeSpec with Matchers with ScalatestRouteTest {

  val mainRoute = new MainRoute()

  "/api/v1/games" - {
    "get games" - {
      "should return empty list if there is no existing game" in {
        Get("/api/v1/games") ~> mainRoute.route ~> check {
          responseAs[String] shouldEqual "[]"
        }
      }
    }
  }
}

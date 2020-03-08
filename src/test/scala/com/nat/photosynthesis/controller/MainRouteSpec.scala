package com.nat.photosynthesis.controller

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FreeSpec, Matchers}
import org.mockito.Mockito._
import akka.http.scaladsl.server.Directives._

class MainRouteSpec extends FreeSpec with Matchers with ScalatestRouteTest with MockitoSugar {

  "/api/v1/games" - {
    "should route to games route" in {
      val mockedGameRoute = mock[GameRoute]
      when(mockedGameRoute.route).thenReturn(get { complete("this is game route")})

      val mainRoute = new MainRoute(mockedGameRoute)

      Get("/api/v1/games") ~> mainRoute.route ~> check {
        responseAs[String] shouldEqual "this is game route"
      }
    }
  }
}

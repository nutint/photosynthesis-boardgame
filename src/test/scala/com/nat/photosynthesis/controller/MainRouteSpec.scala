package com.nat.photosynthesis.controller

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.mockito.MockitoSugar
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class MainRouteSpec extends AnyFreeSpec with Matchers with ScalatestRouteTest with MockitoSugar {

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

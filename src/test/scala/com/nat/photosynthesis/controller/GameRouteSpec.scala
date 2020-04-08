package com.nat.photosynthesis.controller

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{FreeSpec, Matchers}
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import com.nat.photosynthesis.service.GameService
import com.nat.photosynthesis.service.model.engine.GameEngine

import scala.concurrent.Future

class GameRouteSpec extends FreeSpec with Matchers with ScalatestRouteTest with MockitoSugar {
  "/" - {
    "GET" - {
      "should returns empty array if there is no active games" in {
        val gameService = new GameService {
          override def getGames: Future[Either[String, List[GameEngine]]] = Future.successful(Right(List()))
        }
        val gameRoute = new GameRoute(gameService)
        Get() ~> gameRoute.route ~> check {
          responseAs[String] shouldEqual "[]"
        }
      }
    }
  }
}

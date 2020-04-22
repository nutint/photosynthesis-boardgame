package com.nat.photosynthesis.controller

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{FreeSpec, Matchers}
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import com.nat.photosynthesis.service.GameService
import com.nat.photosynthesis.service.model.engine.GameEngine
import com.nat.photosynthesis.service.model._

import scala.concurrent.Future

class GameRouteSpec extends FreeSpec with Matchers with ScalatestRouteTest with MockitoSugar {
  import GameRouteSpec._
  import spray.json._
  import DefaultJsonProtocol._

  "/" - {
    "GET" - {
      "should returns empty array if there is no active games" in {
        val gameService = mock[GameService]
        when(gameService.getGames).thenReturn(Future.successful(Right(List())))
        val gameRoute = new GameRoute(gameService)
        Get() ~> gameRoute.route ~> check {
          responseAs[String] shouldEqual "[]"
        }
      }
    }

    "POST" - {
      "should be able to create game by just specify the player and plant types in the game" in {
        val gameService = mock[GameService]
        val players = List(PlayerJsonEntity("John", "Green"), PlayerJsonEntity("Linda", "Yellow"))
        val servicePlayers = List(Player("John", Green), Player("Linda", Yellow))
//        when(gameService.createGame(servicePlayers)).thenReturn()

        val gameRoute = new GameRoute(gameService)
        Post(
          "/"
//          ,
//          HttpEntity(
//            ContentTypes.`application/json`,
//            players.toJson.compactPrint
//          )
        ) ~> gameRoute.route ~> check {
          status shouldBe StatusCodes.OK
        }
      }
    }
  }
}

object GameRouteSpec {

  import spray.json._
  import DefaultJsonProtocol._

  case class PlayerJsonEntity(name: String, plantType: String)

  implicit val playerJsonEntityFormat = jsonFormat2(PlayerJsonEntity)
}
package com.nat.photosynthesis.controller

import akka.NotUsed
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.model.MediaTypes.`text/event-stream`
import akka.http.scaladsl.model.sse.ServerSentEvent
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.stream.scaladsl.{Sink, Source}
import cats.effect.IO
import com.nat.model.Identifiable
import com.nat.photosynthesis.service.GameService
import com.nat.photosynthesis.service.model._
import com.nat.photosynthesis.service.model.engine.{GameEngine, Registration}
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class GameRouteSpec
  extends FreeSpec
    with Matchers
    with ScalatestRouteTest
    with MockitoSugar {

  import spray.json._
  import DefaultJsonProtocol._
  import GameRouteSpec._
  import akka.http.scaladsl.marshalling.sse.EventStreamMarshalling._
  import akka.http.scaladsl.server.Directives._

  "/" - {

    "GET" - {

      "should returns empty array if there is no active games" in {
        val gameService = mock[GameService[Future]]
        when(gameService.getGames).thenReturn(Future.successful(Right(List())))

        val gameRoute = new GameRoute(gameService)

        Get() ~> gameRoute.route ~> check {
          responseAs[String] shouldEqual "[]"
        }
      }
    }

    "POST" - {

      "should be able to create game by just specify the player and plant types in the game" in {
        val gameService = mock[GameService[Future]]
        val gameEngine = Registration(Nil, ScoringTokenStacks())
        val gameEngineWithId = Identifiable[GameEngine]("newId", gameEngine)
        val players = List(PlayerJsonEntity("John", "Green"), PlayerJsonEntity("Linda", "Yellow"))
        val servicePlayers = List(Player("John", Green), Player("Linda", Yellow))
        when(gameService.createGame(servicePlayers)).thenReturn(Future.successful(Right(gameEngineWithId)))

        val gameRoute = new GameRoute(gameService)

        Post(
          "/",
          HttpEntity(ContentTypes.`application/json`, players.toJson.compactPrint)
        ) ~> gameRoute.route ~> check {
          status shouldBe StatusCodes.Created
        }
      }
    }
  }

  "/game-events" - {
    "playing with event source" in {
      val events = 1.to(3).map(n => ServerSentEvent(n.toString))
      val route = complete(Source(events))
      Get() ~> route ~> check {
        mediaType shouldBe `text/event-stream`
        Await.result(responseEntity.dataBytes.runWith(Sink.seq), 3.seconds) shouldBe events.map(_.data)
      }
    }
    "GET" - {

      "should returns empty array if there is no active games" in {
//        val gameService = mock[GameService[Future]]
//        when(gameService.getGames).thenReturn(Future.successful(Right(List())))
//
//        val gameEventSource: Seq[ServerSentEvent] = (1 to 10).map(n => ServerSentEvent(n.toString))
//        val source: Source[ServerSentEvent, NotUsed] = Source(gameEventSource)
//        val gameRoute = new GameRoute(gameService, source)
//
//        Get("/game-events") ~> gameRoute.route ~> check {
//          mediaType shouldBe `text/event-stream`
//          Await.result(responseEntity.dataBytes.runWith(Sink.seq), 3 seconds) shouldBe gameEventSource.map(_.data)
//        }
      }
    }
  }

  "play with unsafe IO" - {
    "unwrapping IO[A] toBe Try[A]" in {

      val okIO = IO { 1 }
      okIO.unsafeRunSync() shouldBe 1

      val failedIO = IO[Int]{
        val exception = new Exception("error")
        throw exception }
      val thrown = intercept[Exception] {
        failedIO.unsafeRunSync()
      }
      thrown.getMessage shouldBe "error"
    }
  }
}

object GameRouteSpec {

  import spray.json._
  import DefaultJsonProtocol._

  case class PlayerJsonEntity(name: String, plantType: String)

  implicit val playerJsonEntityFormat: RootJsonFormat[PlayerJsonEntity] = jsonFormat2(PlayerJsonEntity.apply)
}

package com.nat.photosynthesis.controller

import java.time.LocalTime
import java.time.format.DateTimeFormatter.ISO_LOCAL_TIME

import akka.NotUsed
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.model.sse.ServerSentEvent
import akka.http.scaladsl.server.Directives._
import akka.stream.scaladsl.Source
import com.nat.model.Identifiable
import com.nat.photosynthesis.service.GameService
import com.nat.photosynthesis.service.model.Player
import com.nat.photosynthesis.service.model.engine.GameEngine

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util._

class GameRoute(gameService: GameService[Future]) {

  import spray.json._
  import DefaultJsonProtocol._
  import JsonFormats._
  import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
  import com.nat.controller.JsonFormats._
  import akka.http.scaladsl.marshalling.sse.EventStreamMarshalling._

  val route =
    path("game-events") {
      get {
        complete {
          Source
            .tick(2 seconds, 2 seconds, NotUsed)
            .map(_ => LocalTime.now())
            .map(time => ServerSentEvent(ISO_LOCAL_TIME.format(time)))
            .keepAlive(1 second, () => ServerSentEvent.heartbeat)
        }
      }
    } ~
    get {
      val games = gameService.getGames
      onComplete(games)(_ => complete("[]"))
    } ~
    post {
      entity(as[List[Player]]) { players =>
        onComplete(gameService.createGame(players)) {
          case scala.util.Success(res) => res match {
            case Right(gameEngineWithId) => complete((
              Created,
              DataResponse[Identifiable[GameEngine]](
                Some(gameEngineWithId),
                Created,
                "Created")
            ))
          }
        }
      }
    }

}

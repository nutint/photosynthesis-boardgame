package com.nat.photosynthesis.controller

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import com.nat.model.Identifiable
import com.nat.photosynthesis.service.GameService
import com.nat.photosynthesis.service.model.Player
import com.nat.photosynthesis.service.model.engine.GameEngine
import scala.util._

class GameRoute(gameService: GameService) {

  import spray.json._
  import DefaultJsonProtocol._
  import com.nat.controller.JsonFormats._
  import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
  import JsonFormats._

  val route =
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
            case Left(errorMessage) => complete((BadRequest, errorMessage))
          }
        }
      }
    }

}

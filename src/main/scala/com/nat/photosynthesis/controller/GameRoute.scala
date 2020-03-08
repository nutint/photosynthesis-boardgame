package com.nat.photosynthesis.controller

import akka.http.scaladsl.server.Directives._
import com.nat.photosynthesis.service.GameService

class GameRoute(gameService: GameService) {

  val route =
    get {
      val games = gameService.getGames
      onComplete(games)(_ => complete("[]"))
    }

}

package com.nat.photosynthesis.controller

import akka.http.scaladsl.server.Directives._

class MainRoute(gameRoute: GameRoute) {

  val route = path("api" / "v1" / "games") {
    gameRoute.route
  }
}

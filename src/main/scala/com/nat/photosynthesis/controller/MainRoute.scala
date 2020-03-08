package com.nat.photosynthesis.controller

import akka.http.scaladsl.server.Directives._

class MainRoute {

  val route = path("api" / "v1" / "games") {
    get {
      complete("[]")
    }
  }
}

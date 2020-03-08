package com.nat.photosynthesis.service

import com.nat.photosynthesis.model.GameEngine

import scala.concurrent.Future

trait GameService {

  def getGames: Future[Either[String, List[GameEngine]]]
}

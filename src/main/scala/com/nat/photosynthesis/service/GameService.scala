package com.nat.photosynthesis.service

import com.nat.photosynthesis.service.model.engine.GameEngine
import com.nat.photosynthesis.service.repository.GameRepository

import scala.concurrent.Future

class GameService(gameRepository: GameRepository) {

  def getGames: Future[Either[String, List[GameEngine]]] = ???
}

package com.nat.photosynthesis.service

import com.nat.model.Identifiable
import com.nat.photosynthesis.service.model.Player
import com.nat.photosynthesis.service.model.engine.GameEngine
import com.nat.photosynthesis.service.repository.GameRepository

import scala.concurrent.Future

class GameService[F[_]](gameRepository: GameRepository) {

  def getGames: F[Either[String, List[GameEngine]]] = ???

  def createGame(players: List[Player]): F[Either[String, Identifiable[GameEngine]]] = ???
}

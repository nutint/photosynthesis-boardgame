package com.nat.photosynthesis.service

import com.nat.model.Identifiable
import com.nat.photosynthesis.service.model.{AddPlayer, Player}
import com.nat.photosynthesis.service.model.engine.GameEngine
import com.nat.photosynthesis.service.repository.GameRepository
import com.nat.photosynthesis.utils.UUIDGenerator

import scala.concurrent.Future

class GameService(gameRepository: GameRepository, uuidGenerator: UUIDGenerator) {
  import com.nat.photosynthesis.service.model.Processor._

  def getGames: Future[Either[String, List[GameEngine]]] = ???

  def createGame(players: List[Player]): Future[Either[String, Identifiable[GameEngine]]] = {
    val initialGameEngine: Either[String, GameEngine] = Right(GameEngine())
    val resultGameEngine: Either[String, GameEngine] = players
      .foldLeft(initialGameEngine)((gameEngine: Either[String, GameEngine], player) => gameEngine.flatMap(_.processCommand(AddPlayer(player))))

    val gameEngineWithId: Either[String, Identifiable[GameEngine]] = resultGameEngine.map(x => {
      val gameId = uuidGenerator.generate()
      Identifiable(gameId, x)
    })
    Future.successful(
      gameEngineWithId
    )
  }
}

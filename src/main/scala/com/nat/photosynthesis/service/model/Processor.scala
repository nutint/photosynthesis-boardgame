package com.nat.photosynthesis.service.model

import com.nat.photosynthesis.service.model.engine.{GameEngine, GameOver, Registration}

object Processor {

  implicit class GameEngineProcessor(gameEngine: GameEngine) {
    def processCommand(gameEngineCommand: GameEngineCommand): Either[String, GameEngine] =
      gameEngine match {
        case game: Registration => game.processCommand(gameEngineCommand)
        case game: GameOver => game.processCommand(gameEngineCommand)
      }

    def processCommands(gameEngineCommands: List[GameEngineCommand]): Either[String, GameEngine] =
      gameEngineCommands
        .foldLeft[Either[String, GameEngine]](Right(gameEngine)) { (currEngine, command) =>
          currEngine.flatMap(_.processCommand(command))
        }
  }

  implicit class RegistrationProcessor(registration: Registration) {
    def processCommand(gameEngineCommand: GameEngineCommand): Either[String, GameEngine] = {
      gameEngineCommand match {
        case AddPlayer(player) => registration.addPlayer(player)
        case StartGame => registration.startGame
        case _ => Left("Bad command")
      }
    }
  }

  implicit class GameOverProcessor(gameOver: GameOver) {
    def processCommand(gameEngineCommand: GameEngineCommand): Either[String, GameEngine] =
      gameEngineCommand match {
        case _ => Left("Bad command")
      }
  }
}

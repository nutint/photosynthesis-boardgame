package com.nat.photosynthesis.service

import com.nat.photosynthesis.repository.GameEngineRepository

class GameEngineCommandHandler(gameEngineRepository: GameEngineRepository) {

  def processCommand(gameEngineCommand: GameEngineCommand): Either[String, GameEngineResult] = {
    gameEngineCommand match {
      case AddGame => addGame
    }
  }

  def addGame: Either[String, GameEngineResult] = Right(GameAdded("mockedId"))

}

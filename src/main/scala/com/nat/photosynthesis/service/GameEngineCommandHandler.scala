package com.nat.photosynthesis.service

import com.nat.photosynthesis.repository.GameEngineRepository
import com.nat.photosynthesis.service.model._

class GameEngineCommandHandler(gameEngineRepository: GameEngineRepository) {

  def processCommand(gameEngineCommand: GameEngineCommand): Either[String, GameEngineResult] = {
    gameEngineCommand match {
      case AddGame => addGame
      case AddPlayer(roomId, name, plantType) => addPlayer(roomId, name, plantType)
    }
  }

  def addGame: Either[String, GameEngineResult] = Right(GameAdded("mockedId"))

  def addPlayer(roomId: String, name: String, plantType: PlantType): Either[String, GameEngineResult] =
    gameEngineRepository
      .getGameById(roomId)
      .flatMap {
        case registration: Registration => registration.addPlayer(Player(name, plantType))
        case _ => Left("Unable to add player at this time")
      }
      .map(_ => PlayerAdded) match {
        case Right(res) => Right(res)
        case Left(reason) => Right(AddPlayerFailed(reason))
      }
}

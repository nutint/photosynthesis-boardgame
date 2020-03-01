package com.nat.photosynthesis.model

sealed trait GameEngine

case class GameEngineRegistrationState(
  players: List[Player]
) extends GameEngine {
  def addPlayer(player: Player): Either[String, GameEngineRegistrationState] =
    if(players.exists(_.plantType == player.plantType)) {
      Left("Unable to add player with same plant type")
    } else if (players.exists(_.name.toLowerCase == player.name.toLowerCase)) {
      Left("Unable to add player with the same name")
    } else {
      Right(copy(players = players :+ player))
    }

  def startGame: Either[String, GameEngineSetupState] = {
    if(players.length <= 1) {
      Left("Cannot start game less than 2 players")
    } else {
      Right(
        GameEngineSetupState(
          playerBoards = players.map(_.initBoard),
          forestBlocks = Nil,
          remainingTokens = TokenStock(Nil, Nil, Nil, Nil)
        ))
    }
  }
}

case class GameEngineSetupState(
  playerBoards: List[PlayerBoard],
  forestBlocks: List[ForestBlock],
  remainingTokens: TokenStock
)

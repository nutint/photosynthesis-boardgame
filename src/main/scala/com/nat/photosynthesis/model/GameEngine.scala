package com.nat.photosynthesis.model

sealed trait GameEngine

case class GameEngineRegistrationState(
  players: List[Player],
  tokenStock: TokenStock = TokenStock(Nil, Nil, Nil, Nil)
) extends GameEngine {
  def addPlayer(player: Player): Either[String, GameEngineRegistrationState] =
    if(players.exists(_.plantType == player.plantType)) {
      Left("Unable to add player with same plant type")
    } else if (players.exists(_.name.toLowerCase == player.name.toLowerCase)) {
      Left("Unable to add player with the same name")
    } else {
      Right(copy(players = players :+ player))
    }

  def setTokenStock(tokenStock: TokenStock) = copy(tokenStock = tokenStock)

  def startGame: Either[String, GameEnginePlacingFirst2TreesState] = {
    if(players.length <= 1) {
      Left("Cannot start game less than 2 players")
    } else {
      Right(
        GameEnginePlacingFirst2TreesState(
          plantingTreePlayer = 0,
          playerBoards = players.map(_.initBoard),
          forestBlocks = Nil,
          tokenStock = tokenStock
        ))
    }
  }
}

case class GameEnginePlacingFirst2TreesState(
  plantingTreePlayer: Int,
  playerBoards: List[PlayerBoard],
  forestBlocks: List[ForestBlock],
  tokenStock: TokenStock
) extends GameEngine {
  def placeTree(playerNo: Int, boardLocation: BoardLocation, smallTree: SmallTree): Either[String, GameEnginePlacingFirst2TreesState] = {
    if(playerNo == plantingTreePlayer) {
      if(boardLocation.isEdgeLocation)
        Right(copy(
          plantingTreePlayer = (playerNo + 1) % (playerBoards.length),
          forestBlocks = forestBlocks :+ boardLocation.toForestBlock(smallTree)
        ))
      else
        Left(s"Cannot place on location (0, 0, 0) since it is not edge location")
    } else {
      Left(s"Not player $playerNo turn yet, currently player $plantingTreePlayer")
    }
  }
}

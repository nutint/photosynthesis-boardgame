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
  def placeTree(playerNo: Int, boardLocation: BoardLocation): Either[String, GameEnginePlacingFirst2TreesState] = {
    if(playerNo == plantingTreePlayer) {
      if(boardLocation.isEdgeLocation)
        if(forestBlocks.exists(_.boardLocation == boardLocation))
          Left("Unable to place to non empty location")
        else {
          Right(copy(
            plantingTreePlayer = (playerNo + 1) % (playerBoards.length),
            forestBlocks = forestBlocks :+ boardLocation.toForestBlock(
              SmallTree(activePlayer.plantType)
            )
          ))
        }
      else {
        val BoardLocation(x, y, z) = boardLocation
        Left(s"Cannot place on location ($x, $y, $z) since it is not edge location")
      }
    } else {
      Left(s"Not player $playerNo turn yet, currently player $plantingTreePlayer")
    }
  }

  def activePlayer = playerBoards(plantingTreePlayer).player

  def startPlaying: Either[String, GameEnginePlaying] = {
    if(forestBlocks.length == playerBoards.length * 2)
      Right(GameEnginePlaying(
        actionPlayer = 0,
        startingPlayer = 0,
        sunLocation = SunLocation0,
        day = 0,
        playerBoards = playerBoards,
        forestBlocks = forestBlocks,
        tokenStock = tokenStock
      ))
    else
      Left("Some player need to place their small tree")
  }
}

case class GameEnginePlaying(
  actionPlayer: Int,
  startingPlayer: Int,
  sunLocation: SunLocation,
  day: Int,
  playerBoards: List[PlayerBoard],
  forestBlocks: List[ForestBlock],
  tokenStock: TokenStock
)

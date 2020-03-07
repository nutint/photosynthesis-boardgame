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

  def setTokenStock(tokenStock: TokenStock): GameEngineRegistrationState = copy(tokenStock = tokenStock)

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
            plantingTreePlayer = (playerNo + 1) % playerBoards.length,
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

  def activePlayer: Player = playerBoards(plantingTreePlayer).player

  def startPlaying: Either[String, GameEnginePlaying] = {
    if(forestBlocks.length >= playerBoards.length * 2) {
      val allPlaced2Trees = forestBlocks
        .groupBy(_.plantItem.plantType)
        .map(_._2.length == 2)
        .forall(_ == true)

      if(allPlaced2Trees) {
        val calculatedScoreBoard = playerBoards
          .map { board =>
            val playerBoardScore = forestBlocks
              .filter(_.plantItem.plantType == board.player.plantType)
              .map(_.calculateScore(SunLocation0, forestBlocks))
              .sum
            board.copy(sun = playerBoardScore)
          }
        Right(GameEnginePlaying(
          actionPlayer = 0,
          startingPlayer = 0,
          sunLocation = SunLocation0,
          day = 0,
          playerBoards = calculatedScoreBoard,
          forestBlocks = forestBlocks,
          tokenStock = tokenStock
        ))
      } else {
        Left("Cannot start the game: all players must place only 2 trees")
      }

    }
    else
      Left("Cannot start the game: all players must place 2 trees")
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
) extends GameEngine {
  def lastDay = 4
  def passNextPlayer: GameEngine = {
    val nextPlayer = (actionPlayer + 1) % playerBoards.length
    val nextStartingPlayer = (startingPlayer + 1) % playerBoards.length
    val endRound = nextPlayer == startingPlayer
    val endDay = sunLocation.next == SunLocation0
    val calculatedStartingPlayer = if(endRound) nextStartingPlayer else startingPlayer
    val calculatedNextPlayer = if(endRound) calculatedStartingPlayer else nextPlayer
    val calculatedSunLocation = if(endRound) sunLocation.next else sunLocation
    val calculatedDay = if(endDay) day + 1 else day
    val endGame = day + 1 == lastDay
    if(endGame && endDay) GameEngineOver(playerBoards, forestBlocks)
    else
      copy(
        actionPlayer = calculatedNextPlayer,
        startingPlayer = calculatedStartingPlayer,
        sunLocation = calculatedSunLocation,
        day = calculatedDay
      )
  }

  def playerSeedPlant(player: Player, motherLocation: BoardLocation, seedLocation: BoardLocation): Either[String,GameEnginePlaying] = {
    val sunLocations = List(SunLocation0, SunLocation1, SunLocation2)
    if(!playerBoards.exists(_.player == player)) {
      Left("Unable to seed: Player not found")
    } else if (forestBlocks.exists(_.boardLocation == seedLocation)) {
      Left("Unable to seed: Target location is not empty")
    } else if (!sunLocations.exists(sl => motherLocation.isSameLine(seedLocation, sl))) {
      Left("Unable to seed: Not the same line")
    } else {
      forestBlocks
        .find(fb => fb.boardLocation == motherLocation)
        .map {
          case ForestBlock(_, plantItem) if plantItem.plantType == player.plantType => plantItem match {
            case _: CooledDownPlantItem => Left("Unable to seed: Plant is in cool down")
            case sa: SeedAble =>
              if(motherLocation.inRadius(seedLocation, plantItem.height)) {
                val updatedForestBlocks = forestBlocks.map {
                  case fb @ ForestBlock(bl, pi) if bl == motherLocation && pi.plantType == player.plantType => fb.copy(plantItem = sa.seed)
                  case a => a
                }
                Right(copy(forestBlocks = updatedForestBlocks :+ ForestBlock(seedLocation, Seed(player.plantType))))
              } else {
                Left("Unable to seed: Out of range")
              }
            case _ => Left("Unable to seed: Cannot seed")
          }
          case ForestBlock(_, plantItem) if plantItem.plantType != player.plantType => Left("Unable to seed: Not player's plant")
        }
        .getOrElse(Left("Unable to seed: Plant not found"))
    }
  }

  def grow(player: Player, bl: BoardLocation): Either[String,GameEnginePlaying] =
    forestBlocks
      .find(_.boardLocation == bl )
      .map {
        case fb: ForestBlock if !fb.isOwnedBy(player) => Left(s"Not own by player ${player.name}")
        case fb: ForestBlock if fb.isOwnedBy(player) => fb.plantItem match {
          case _: CooledDownPlantItem => Left("Cooling down")
          case _: LargeTree => Left("Already large tree")
          case ga: GrowAble =>
            playerBoards
              .find(_.player == player)
              .map(_.withdrawResource(ga.growResource)) match {
              case None => Left("Player not found")
              case Some(e) => e
                .map { newPb =>
                  copy(
                    playerBoards = playerBoards.map(opb => if(opb.player == player) newPb else opb),
                    forestBlocks = forestBlocks.map(fb => if(fb.boardLocation == bl) fb.copy(plantItem = ga.grow) else fb )
                  )
                }
            }

//            Either.cond(optionalPlayer.isDefined, optionalPlayer.get, "Unable to grow: Player not found")
//              .flatMap(_)
//              .map { newPb =>
//                copy(
//                  playerBoards = playerBoards.map( opb => if(opb.player == player) newPb else opb )
//                )
//              }
//              .getOrElse(Left("Unable to grow: Player not found"))
        }
      }
      .getOrElse(Left(s"No plant here"))

}

case class GameEngineOver(
  playerBoards: List[PlayerBoard],
  forestBlocks: List[ForestBlock]
) extends GameEngine

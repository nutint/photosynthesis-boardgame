package com.nat.photosynthesis.model

sealed trait GameEngine

case class Registration(
  players: List[Player],
  tokenStock: TokenStock = TokenStock(Nil, Nil, Nil, Nil)
) extends GameEngine {
  def addPlayer(player: Player): Either[String, Registration] =
    if(players.exists(_.plantType == player.plantType)) {
      Left("Unable to add player with same plant type")
    } else if (players.exists(_.name.compareToIgnoreCase(player.name) == 0)) {
      Left("Unable to add player with the same name")
    } else {
      Right(copy(players = players :+ player))
    }

  def setTokenStock(tokenStock: TokenStock): Registration = copy(tokenStock = tokenStock)

  def startGame: Either[String, SettingUp] = {
    if(players.length < 2) {
      Left("Cannot start game less than 2 players")
    } else {
      Right(
        SettingUp(
          activePlayerPosition = 0,
          playerBoards = players.map(_.initBoard),
          forestBlocks = Nil,
          tokenStock = tokenStock
        ))
    }
  }
}

case class SettingUp(
  activePlayerPosition: Int,
  playerBoards: List[PlayerBoard],
  forestBlocks: List[Block],
  tokenStock: TokenStock
) extends GameEngine {
  def placeTree(playerNo: Int, boardLocation: Location): Either[String, SettingUp] = {
    if(playerNo == activePlayerPosition) {
      if(boardLocation.isEdgeLocation)
        if(forestBlocks.exists(_.location == boardLocation))
          Left("Unable to place to non empty location")
        else {
          Right(copy(
            activePlayerPosition = (playerNo + 1) % playerBoards.length,
            forestBlocks = forestBlocks :+ boardLocation.toForestBlock(
              SmallTree(activePlayer.plantType)
            )
          ))
        }
      else {
        val Location(x, y, z) = boardLocation
        Left(s"Cannot place on location ($x, $y, $z) since it is not edge location")
      }
    } else {
      Left(s"Not player $playerNo turn yet, currently player $activePlayerPosition")
    }
  }

  def activePlayer: Player = playerBoards(activePlayerPosition).player

  def startPlaying: Either[String, Playing] = {
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
        Right(Playing(
          activePlayerPosition = 0,
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

case class Playing(
  activePlayerPosition: Int,
  startingPlayer: Int,
  sunLocation: SunLocation,
  day: Int,
  playerBoards: List[PlayerBoard],
  forestBlocks: List[Block],
  tokenStock: TokenStock
) extends GameEngine {
  def lastDay = 4
  def passNextPlayer: GameEngine = {
    val nextPlayer = (activePlayerPosition + 1) % playerBoards.length
    val nextStartingPlayer = (startingPlayer + 1) % playerBoards.length
    val endRound = nextPlayer == startingPlayer
    val endDay = sunLocation.next == SunLocation0
    val calculatedStartingPlayer = if(endRound) nextStartingPlayer else startingPlayer
    val calculatedNextPlayer = if(endRound) calculatedStartingPlayer else nextPlayer
    val calculatedSunLocation = if(endRound) sunLocation.next else sunLocation
    val calculatedDay = if(endDay) day + 1 else day
    val endGame = day + 1 == lastDay
    if(endGame && endDay) GameOver(playerBoards, forestBlocks)
    else
      copy(
        activePlayerPosition = calculatedNextPlayer,
        startingPlayer = calculatedStartingPlayer,
        sunLocation = calculatedSunLocation,
        day = calculatedDay
      )
  }

  def playerSeedPlant(player: Player, motherLocation: Location, seedLocation: Location): Either[String,Playing] = {
    val sunLocations = List(SunLocation0, SunLocation1, SunLocation2)
    if(!playerBoards.exists(_.player == player)) {
      Left("Unable to seed: Player not found")
    } else if (forestBlocks.exists(_.location == seedLocation)) {
      Left("Unable to seed: Target location is not empty")
    } else if (!sunLocations.exists(sl => motherLocation.isSameLine(seedLocation, sl))) {
      Left("Unable to seed: Not the same line")
    } else {
      forestBlocks
        .find(fb => fb.location == motherLocation)
        .map {
          case Block(_, plantItem) if plantItem.plantType == player.plantType => plantItem match {
            case _: CoolingDownPlant => Left("Unable to seed: Plant is in cool down")
            case sa: SeedAble =>
              if(motherLocation.inRadius(seedLocation, plantItem.height)) {
                val updatedForestBlocks = forestBlocks.map {
                  case fb @ Block(bl, pi) if bl == motherLocation && pi.plantType == player.plantType => fb.copy(plantItem = sa.seed)
                  case a => a
                }
                Right(copy(forestBlocks = updatedForestBlocks :+ Block(seedLocation, Seed(player.plantType))))
              } else {
                Left("Unable to seed: Out of range")
              }
            case _ => Left("Unable to seed: Cannot seed")
          }
          case Block(_, plantItem) if plantItem.plantType != player.plantType => Left("Unable to seed: Not player's plant")
        }
        .getOrElse(Left("Unable to seed: Plant not found"))
    }
  }

  def grow(player: Player, bl: Location): Either[String,Playing] =
    forestBlocks
      .find(_.location == bl )
      .map {
        case fb: Block if !fb.isOwnedBy(player) => Left(s"Not own by player ${player.name}")
        case fb: Block if fb.isOwnedBy(player) => fb.plantItem match {
          case _: CoolingDownPlant => Left("Cooling down")
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
                    forestBlocks = forestBlocks.map(fb => if(fb.location == bl) fb.copy(plantItem = ga.grow) else fb )
                  )
                }
            }
        }
      }
      .getOrElse(Left(s"No plant here"))

  def buyItem(player: Player, plantItem: Plant): Either[String, Playing] =
    if(plantItem.plantType != player.plantType) Left("Cannot buy different species")
    else {
      playerBoards.find(_.player == player)
        .map { playerBoard =>
          playerBoard
            .buy(plantItem)
            .map(newBoard => copy(
              playerBoards = playerBoards.map(toBeUpdatedPlayerBoard => if(toBeUpdatedPlayerBoard == playerBoard) newBoard else toBeUpdatedPlayerBoard))
            )
        }
        .getOrElse(Left("Player not found"))
    }
}

case class GameOver(
  playerBoards: List[PlayerBoard],
  forestBlocks: List[Block]
) extends GameEngine

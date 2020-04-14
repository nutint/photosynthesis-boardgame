package com.nat.photosynthesis.service.model.engine

import com.nat.photosynthesis.service.model._

sealed trait GameEngine

case class Registration(
  players: List[Player],
  scoringTokenStacks: ScoringTokenStacks = ScoringTokenStacks(Nil, Nil, Nil, Nil)
) extends GameEngine {
  def addPlayer(player: Player): Either[String, Registration] =
    if(players.exists(_.plantType == player.plantType)) {
      Left("Unable to add player with same plant type")
    } else if (players.exists(_.name.compareToIgnoreCase(player.name) == 0)) {
      Left("Unable to add player with the same name")
    } else {
      Right(copy(players = players :+ player))
    }

  def setScoringTokenStacks(scoringTokenStacks: ScoringTokenStacks): Registration = copy(scoringTokenStacks = scoringTokenStacks)

  def startGame: Either[String, SettingUp] = {
    if(players.length < 2) {
      Left("Cannot start game less than 2 players")
    } else {
      Right(
        SettingUp(
          activePlayerPosition = 0,
          playerBoards = players.map(_.initBoard),
          blocks = Nil,
          scoringTokenStacks = scoringTokenStacks
        ))
    }
  }
}

case class SettingUp(
  activePlayerPosition: Int,
  playerBoards: List[PlayerBoard],
  blocks: List[Block],
  scoringTokenStacks: ScoringTokenStacks
) extends GameEngine {
  def placeTree(playerNo: Int, location: Location): Either[String, SettingUp] = {
    if(playerNo == activePlayerPosition) {
      if(location.isExternalEdge)
        if(blocks.exists(_.location == location))
          Left("Unable to place to non empty location")
        else {
          Right(copy(
            activePlayerPosition = (playerNo + 1) % playerBoards.length,
            blocks = blocks :+ location.toForestBlock(
              SmallTree(activePlayer.plantType)
            )
          ))
        }
      else {
        val Location(x, y, z) = location
        Left(s"Cannot place on location ($x, $y, $z) since it is not external edge")
      }
    } else {
      Left(s"Not player $playerNo turn yet, currently player $activePlayerPosition")
    }
  }

  def activePlayer: Player = playerBoards(activePlayerPosition).player

  def startPlaying: Either[String, Playing] = {
    if(blocks.length >= playerBoards.length * 2) {
      val allPlaced2Trees = blocks
        .groupBy(_.plantItem.plantType)
        .map(_._2.length == 2)
        .forall(_ == true)

      if(allPlaced2Trees) {
        val calculatedScoreBoard = playerBoards
          .map { board =>
            val playerBoardScore = blocks
              .filter(_.plantItem.plantType == board.player.plantType)
              .map(_.calculateScore(SunLocation0, blocks))
              .sum
            board.copy(lightPoints = playerBoardScore)
          }
        Right(Playing(
          activePlayerPosition = 0,
          firstPlayerTokenPosition = 0,
          sunLocation = SunLocation0,
          day = 0,
          playerBoards = calculatedScoreBoard,
          blocks = blocks,
          scoringTokenStacks = scoringTokenStacks
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
  firstPlayerTokenPosition: Int,
  sunLocation: SunLocation,
  day: Int,
  playerBoards: List[PlayerBoard],
  blocks: List[Block],
  scoringTokenStacks: ScoringTokenStacks
) extends GameEngine {
  def lastDay = 4
  def passNextPlayer: GameEngine = {
    val nextActivePlayerPosition = (activePlayerPosition + 1) % playerBoards.length
    val nextFirstPlayerTokenPosition = (firstPlayerTokenPosition + 1) % playerBoards.length

    val isRoundEnded = nextActivePlayerPosition == firstPlayerTokenPosition

    val nextMoveStartingPlayer = if(isRoundEnded) nextFirstPlayerTokenPosition else firstPlayerTokenPosition
    val nextMoveActivePlayer = if(isRoundEnded) nextMoveStartingPlayer else nextActivePlayerPosition
    val nextMoveSunLocation = if(isRoundEnded) sunLocation.next else sunLocation

    val isDayEnded = sunLocation.next == SunLocation0
    val nextMoveDay = if(isDayEnded) day + 1 else day
    val isGameEnded = day + 1 == lastDay

    if(isGameEnded && isDayEnded)
      GameOver(playerBoards, blocks)
    else
      copy(
        activePlayerPosition = nextMoveActivePlayer,
        firstPlayerTokenPosition = nextMoveStartingPlayer,
        sunLocation = nextMoveSunLocation,
        day = nextMoveDay
      )
  }

  def playerSeedPlant(player: Player, motherLocation: Location, seedLocation: Location): Either[String,Playing] = {
    val sunLocations = List(SunLocation0, SunLocation1, SunLocation2)
    if(!playerBoards.exists(_.player == player)) {
      Left("Unable to seed: Player not found")
    } else if (blocks.exists(_.location == seedLocation)) {
      Left("Unable to seed: Target location is not empty")
    } else if (!sunLocations.exists(sl => motherLocation.isSameLine(seedLocation, sl))) {
      Left("Unable to seed: Not the same line")
    } else {
      blocks
        .find(fb => fb.location == motherLocation)
        .map {
          case Block(_, plantItem) if plantItem.plantType == player.plantType => plantItem match {
            case _: CoolingDownPlant => Left("Unable to seed: Plant is in cool down")
            case sa: SeedAble =>
              if(motherLocation.inRadius(seedLocation, plantItem.height)) {
                val updatedForestBlocks = blocks.map {
                  case fb @ Block(bl, pi) if bl == motherLocation && pi.plantType == player.plantType => fb.copy(plantItem = sa.seed)
                  case a => a
                }
                Right(copy(blocks = updatedForestBlocks :+ Block(seedLocation, Seed(player.plantType))))
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
    blocks
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
                    blocks = blocks.map(fb => if(fb.location == bl) fb.copy(plantItem = ga.grow) else fb )
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
  blocks: List[Block]
) extends GameEngine

object GameEngine {
  def apply(): GameEngine = Registration(Nil, ScoringTokenStacks(Nil, Nil, Nil, Nil))
}
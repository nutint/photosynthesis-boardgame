package com.nat.photosynthesis.model

import org.scalatest.{Assertion, FreeSpec, Matchers}

class GameEngineSpec extends FreeSpec with Matchers
{

  private val john: Player = Player(name = "John", plantType = Green)
  private val rose: Player = Player(name = "Rose", plantType = Yellow)
  val johnAndRose: List[Player] = john :: rose :: Nil

  "GameEngineRegistrationState" - {
    val emptyGameEngineState = Registration(players = Nil)
    val onlyJohnGameEngineState = Registration(
      players = Player(
        name = "John",
        plantType = Green
      ) :: Nil
    )

    val johnWithRoseGameEngine = Registration(
      players = johnAndRose
    )

    "addPlayer" - {
      "should be able to add player when there is no player in the game" in {

        emptyGameEngineState
          .addPlayer(
            Player(
              name = "John",
              plantType = Green
            )) shouldBe
          Right(onlyJohnGameEngineState)
      }

      "should not be able to add player with same plant type" in {
        onlyJohnGameEngineState
          .addPlayer(
            Player("Rose", Green)
          ) shouldBe
          Left("Unable to add player with same plant type")
      }

      "should not be able to add player with same name" in {
        onlyJohnGameEngineState
          .addPlayer(Player("John", Blue)
        ) shouldBe
        Left("Unable to add player with the same name")
      }
    }

    "setTokenStock" - {
      "should be able to set TokenStock" in {
        val tokenStock = TokenStock(Nil, Nil, Nil, List(ScoringTokenTierFour(1)))
        onlyJohnGameEngineState
          .setTokenStock(tokenStock) shouldBe
        onlyJohnGameEngineState.copy(tokenStock =  tokenStock)
      }
    }

    "startGame" - {
      "should not be able to start when there is only 1 player" in {
        onlyJohnGameEngineState.startGame shouldBe Left("Cannot start game less than 2 players")
      }

      "should be able to start the game when there is 2 player" in {
        johnWithRoseGameEngine.startGame shouldBe Right(
          SettingUp(
            plantingTreePlayer = 0,
            playerBoards = johnWithRoseGameEngine.players.map(_.initBoard),
            forestBlocks = Nil,
            tokenStock = TokenStock(Nil, Nil, Nil, Nil)
          )
        )
      }

      "should transfer TokenStock from Registration state" in {
        val tokenStock = TokenStock(Nil, Nil, Nil, List(ScoringTokenTierFour(12)))
        johnWithRoseGameEngine
          .setTokenStock(tokenStock).startGame shouldBe Right(
          SettingUp(
            plantingTreePlayer = 0,
            playerBoards = johnWithRoseGameEngine.players.map(_.initBoard),
            forestBlocks = Nil,
            tokenStock = tokenStock
          )
        )
      }
    }
  }

  "GameEnginePlacingFirst2TreesState" - {

    val nonPlayerPlaceTreeYet = SettingUp(
      plantingTreePlayer = 0,
      playerBoards = johnAndRose.map(_.initBoard),
      forestBlocks = Nil,
      tokenStock = TokenStock()
    )

    "activePlayer" - {
      "should work correctly" in {
        nonPlayerPlaceTreeYet.activePlayer shouldBe john
      }
    }

    "placeTree" - {
      val boardLocation = Location(0, 3, 3)
      val smallTree = SmallTree(Green)

      def verifySuccessAttributes(currentPlayerPos: Int, actionPlayer: Int, assertFn: SettingUp => Assertion): Assertion = {
        nonPlayerPlaceTreeYet
          .copy(plantingTreePlayer = currentPlayerPos)
          .placeTree(actionPlayer, boardLocation) match {
          case Right(ns) => ns match {
            case gs: SettingUp => assertFn(gs)
            case _ => assert(false)
          }
          case _ => assert(false)
        }
      }

      "active player do the place" - {

        "should have new location available in forestBlock and correct tree type" in {
          verifySuccessAttributes(0, 0, _.forestBlocks should contain(boardLocation.toForestBlock(smallTree)))
        }

        "should move to next player" in {
          verifySuccessAttributes(0, 0, _.plantingTreePlayer shouldBe 1)
        }

        "should move to first player if the current player is the last one" in {
          verifySuccessAttributes(1, 1, _.plantingTreePlayer shouldBe 0)
        }

      }

      "should not allow non-active player to plant the tree" in {
        nonPlayerPlaceTreeYet
          .placeTree(1, boardLocation) match {
          case Left(msg) => msg shouldBe s"Not player 1 turn yet, currently player ${nonPlayerPlaceTreeYet.plantingTreePlayer}"
          case _ => assert(false)
        }
      }

      "should not allow place any plant on the non-edge location" in {
        nonPlayerPlaceTreeYet
          .placeTree(0, Location(0, 0, 0)) match {
          case Left(msg) => msg shouldBe s"Cannot place on location (0, 0, 0) since it is not edge location"
          case _ => assert(false)
        }
      }

      "should not allow to place on the same location" in {
        Right[String, SettingUp](nonPlayerPlaceTreeYet)
          .flatMap(_.placeTree(0, boardLocation))
          .flatMap(_.placeTree(1, boardLocation)) match {
          case Left(msg) => msg shouldBe "Unable to place to non empty location"
          case _ => assert(false)
        }
      }
    }

    "startPlaying" - {

      "should allow start playing if all players placed 2 trees" in {
        nonPlayerPlaceTreeYet.startPlaying shouldBe Left("Cannot start the game: all players must place 2 trees")
      }

      "should not allowed player to start playing if non all player not placed 2 trees yet" in {
        Right[String, SettingUp](nonPlayerPlaceTreeYet)
          .flatMap(_.placeTree(0, Location(0, 3, 3)))
          .flatMap(_.startPlaying) shouldBe Left("Cannot start the game: all players must place 2 trees")
      }

      "should allow start playing if all player are finished place 2 trees yet" in {
        val placedTreeGameState = Right[String, SettingUp](nonPlayerPlaceTreeYet)
          .flatMap(_.placeTree(0, Location(0, 3, 3)))
          .flatMap(_.placeTree(1, Location(3, 3, 0)))
          .flatMap(_.placeTree(0, Location(-3, 3, 0)))
          .flatMap(_.placeTree(1, Location(0, -3, -3)))

        val extractedPlacedTreeGameState = placedTreeGameState.getOrElse(null)

        val calculatedScoreBoard = extractedPlacedTreeGameState.playerBoards
            .map { board =>
              val playerBoardScore = extractedPlacedTreeGameState
                .forestBlocks
                .filter(_.plantItem.plantType == board.player.plantType)
                .map(_.calculateScore(SunLocation0, extractedPlacedTreeGameState.forestBlocks))
                .sum
              board.copy(sun = playerBoardScore)
            }

        placedTreeGameState
          .flatMap(_.startPlaying) shouldBe Right(
            Playing(
              actionPlayer = 0,
              startingPlayer = 0,
              sunLocation = SunLocation0,
              day = 0,
              playerBoards = calculatedScoreBoard,
              forestBlocks = extractedPlacedTreeGameState.forestBlocks,
              tokenStock = extractedPlacedTreeGameState.tokenStock
            )
          )
      }

      "should not allow if start playing when all player does not have 2 trees" in {
        Right[String, SettingUp](nonPlayerPlaceTreeYet)
          .flatMap(_.placeTree(0, Location(0, 3, 3)))
          .flatMap(_.placeTree(1, Location(3, 3, 0)))
          .flatMap(_.placeTree(0, Location(-3, 3, 0)))
          .flatMap(_.placeTree(1, Location(0, -3, -3)))
          .flatMap(_.placeTree(0, Location(-3, -3, 0)))
          .flatMap(_.startPlaying) shouldBe Left("Cannot start the game: all players must place only 2 trees")
      }
    }
  }

  "GameEnginePlaying" - {
    val initialState = Playing(
      actionPlayer = 0,
      startingPlayer = 0,
      sunLocation = SunLocation0,
      day = 0,
      playerBoards = List(Player("John", Green), Player("Doe", Blue), Player("Sarah", Orange)).map(_.initBoard),
      forestBlocks = Nil,
      tokenStock = TokenStock()
    )

    "passNextPlayer" - {
      "should remain player token at the same player and make next player actionable" in {
        initialState.passNextPlayer shouldBe initialState.copy(actionPlayer = 1)
      }
      "should set next and starting player to the same, and move sun location if the last player of the round is end turn" in {
        initialState.copy(actionPlayer = 2, startingPlayer = 0).passNextPlayer shouldBe initialState.copy(actionPlayer = 1, startingPlayer = 1, sunLocation = SunLocation1)
        initialState.copy(actionPlayer = 1, startingPlayer = 2).passNextPlayer shouldBe initialState.copy(actionPlayer = 0, startingPlayer = 0, sunLocation = SunLocation1)
      }
      "should move player token, move sun, increase day, and set next player as first player of next round" in {
        initialState.copy(actionPlayer = 1, startingPlayer = 2, sunLocation = SunLocation5).passNextPlayer shouldBe
          initialState.copy(actionPlayer = 0, startingPlayer = 0, sunLocation = SunLocation0, day = 1)
      }
      "should end the game if the sun come back to the starting point and there is the last round token remove from the board" in {
        val Playing(_, _, _, _, playerBoard, forestBlock, _) = initialState
        initialState.copy(actionPlayer = 1, startingPlayer = 2, sunLocation = SunLocation5, day = 3).passNextPlayer shouldBe
          GameOver(playerBoard, forestBlock)
      }
    }
    "seed" - {
      "should fail if player does not exists" in {
        val john = Player("John", Blue)
        val forestBlocks = List(Block(1, 1, 0, SmallTree(Green)))
        val playerBoards = List(john.initBoard)
        val motherLocation = Location(1, 1, 0)
        val seedLocation = Location(0, 0, 0)
        initialState
          .copy(forestBlocks = forestBlocks, playerBoards = playerBoards)
          .playerSeedPlant(Player("other", Blue), motherLocation, seedLocation) shouldBe Left("Unable to seed: Player not found")
      }
      "should fail if the seed location is not empty" in {
        val john = Player("John", Blue)
        val forestBlocks = List(Block(1, 1, 0, CoolingDownSmallTree(Blue)), Block(0, 0, 0, CoolingDownSmallTree(Blue)))
        val playerBoards = List(john.initBoard)
        val motherLocation = Location(1, 1, 0)
        val seedLocation = Location(0, 0, 0)
        initialState
          .copy(forestBlocks = forestBlocks, playerBoards = playerBoards)
          .playerSeedPlant(john, motherLocation, seedLocation) shouldBe Left("Unable to seed: Target location is not empty")
      }
      "should success if the location is the same line and in seeding range" in {
        val john = Player("John", Blue)
        val forestBlocks = List(Block(1, 1, 0, LargeTree(Blue)))
        val playerBoards = List(john.initBoard)
        val motherLocation = Location(1, 1, 0)
        val seedLocation = Location(0, 2, 2)
        initialState
          .copy(forestBlocks = forestBlocks, playerBoards = playerBoards)
          .playerSeedPlant(john, motherLocation, seedLocation) shouldBe Left("Unable to seed: Not the same line")
      }
      "should fail if the plant is not the same plant from player" in {
        val john = Player("John", Blue)
        val forestBlocks = List(Block(0, 0, 0, SmallTree(Green)))
        val playerBoards = List(john.initBoard)
        val motherLocation = Location(0, 0, 0)
        val seedLocation = Location(0, 1, 1)
        initialState
          .copy(forestBlocks = forestBlocks, playerBoards = playerBoards)
          .playerSeedPlant(john, motherLocation, seedLocation) shouldBe Left("Unable to seed: Not player's plant")
      }
      "should fail when the mother location does not exists on the board" in {
        val john = Player("John", Blue)
        val forestBlocks = List(Block(1, 1, 0, SmallTree(Green)))
        val playerBoards = List(john.initBoard)
        val motherLocation = Location(0, 0, 0)
        val seedLocation = Location(0, 0, 0)
        initialState
          .copy(forestBlocks = forestBlocks, playerBoards = playerBoards)
          .playerSeedPlant(john, motherLocation, seedLocation) shouldBe Left("Unable to seed: Plant not found")
      }
      "should fail if the plant is in cool down" in {
        val john = Player("John", Blue)
        val forestBlocks = List(Block(1, 1, 0, CoolingDownSmallTree(Blue)))
        val playerBoards = List(john.initBoard)
        val motherLocation = Location(1, 1, 0)
        val seedLocation = Location(0, 0, 0)
        initialState
          .copy(forestBlocks = forestBlocks, playerBoards = playerBoards)
          .playerSeedPlant(john, motherLocation, seedLocation) shouldBe Left("Unable to seed: Plant is in cool down")
      }
      "should fail if the the mother plant is still cool down" in {
        val john = Player("John", Blue)
        val forestBlocks = List(Block(1, 1, 0, CoolingDownSmallTree(Blue)))
        val playerBoards = List(john.initBoard)
        val motherLocation = Location(1, 1, 0)
        val seedLocation = Location(0, 0, 0)
        initialState
          .copy(forestBlocks = forestBlocks, playerBoards = playerBoards)
          .playerSeedPlant(john, motherLocation, seedLocation) shouldBe Left("Unable to seed: Plant is in cool down")
      }
      "should fail if the mother plant is seed" in {
        val john = Player("John", Blue)
        val forestBlocks = List(Block(1, 1, 0, Seed(Blue)))
        val playerBoards = List(john.initBoard)
        val motherLocation = Location(1, 1, 0)
        val seedLocation = Location(0, 0, 0)
        initialState
          .copy(forestBlocks = forestBlocks, playerBoards = playerBoards)
          .playerSeedPlant(john, motherLocation, seedLocation) shouldBe Left("Unable to seed: Cannot seed")
      }
      "should fail if the location if out of range" in {
        val john = Player("John", Blue)
        val forestBlocks = List(Block(1, 1, 0, MediumTree(Blue)))
        val playerBoards = List(john.initBoard)
        val motherLocation = Location(1, 1, 0)
        val seedLocation = Location(-2, 1, 3)
        initialState
          .copy(forestBlocks = forestBlocks, playerBoards = playerBoards)
          .playerSeedPlant(john, motherLocation, seedLocation) shouldBe Left("Unable to seed: Out of range")
      }
      "should success if all condition is satisfied" in {
        val john = Player("John", Blue)
        val sa = MediumTree(Blue)
        val forestBlocks = List(Block(1, 1, 0, sa))
        val playerBoards = List(john.initBoard)
        val motherLocation = Location(1, 1, 0)
        val seedLocation = Location(-1, 1, 2)
        val mockedInitialState = initialState
          .copy(forestBlocks = forestBlocks, playerBoards = playerBoards)

        val expectedInitialState = {
          val updatedForestBlocks = mockedInitialState.forestBlocks.map {
            case fb @ Block(bl, pi) if bl == motherLocation && pi.plantType == john.plantType => fb.copy(plantItem = sa.seed)
            case a => a
          }
          Right(mockedInitialState.copy(forestBlocks = updatedForestBlocks :+ Block(seedLocation, Seed(john.plantType))))
        }
        mockedInitialState
          .playerSeedPlant(john, motherLocation, seedLocation) shouldBe expectedInitialState
      }
    }
    "grow" - {
      "should fail if there is no plant in the location" in {
        val john = Player("John", Blue)
        val forestBlocks = Nil
        initialState.copy(forestBlocks = forestBlocks)
          .grow(john, Location(1, 1, 0)) shouldBe Left("No plant here")
      }
      "should fail if the plant is not owned by player" in {
        val john = Player("John", Blue)
        val forestBlocks = List(Block(1, 1, 0, MediumTree(Green)))
        initialState.copy(forestBlocks = forestBlocks)
          .grow(john, Location(1, 1, 0)) shouldBe Left("Not own by player John")
      }
      "should fail if the plant is during cool down" in {
        val john = Player("John", Blue)
        val forestBlocks = List(Block(1, 1, 0, CoolingDownMediumTree(Blue)))
        initialState.copy(forestBlocks = forestBlocks)
          .grow(john, Location(1, 1, 0)) shouldBe Left("Cooling down")
      }
      "should fail if the tree is already large tree" in {
        val john = Player("John", Blue)
        val forestBlocks = List(Block(1, 1, 0, LargeTree(Blue)))
        initialState.copy(forestBlocks = forestBlocks)
          .grow(john, Location(1, 1, 0)) shouldBe Left("Already large tree")
      }
      "should fail if player is not in the board" in {
        val john = Player("John", Blue)
        val forestBlocks = List(Block(1, 1, 0, SmallTree(Blue)))
        initialState
          .copy(
            forestBlocks = forestBlocks,
            playerBoards = List(john.copy(name = "Other").initBoard)
          )
          .grow(john, Location(1, 1, 0)) shouldBe Left("Player not found")
      }

      "should fail if there is no available bigger tree in the stock" in {
        val john = Player("John", Blue)
        val forestBlocks = List(Block(1, 1, 0, MediumTree(Blue)))
        val playerBoards = List(john.initBoard.copy(stock = Nil).copy(sun = 9))
        initialState
          .copy(
            forestBlocks = forestBlocks,
            playerBoards = playerBoards)
          .grow(john, Location(1, 1, 0)) shouldBe Left("Not enough tree/seed in stock")
      }
      "should fail if there is available bigger tree but not enough sun" in {
        val john = Player("John", Blue)
        val forestBlocks = List(Block(1, 1, 0, MediumTree(Blue)))
        val playerBoards = List(john.initBoard.copy(stock = Nil).copy(sun = 0))
        initialState
          .copy(
            forestBlocks = forestBlocks,
            playerBoards = playerBoards)
          .grow(john, Location(1, 1, 0)) shouldBe Left("Not enough sun")
      }
      "should success if there is enough sun, have available bigger tree, also place replaced tree in available space" in {
        val john = Player("John", Blue)
        val johnsBoard = john.initBoard.copy(
          stock = List(LargeTree(Blue)),
          sun = 3,
          store = PlantStore(Blue).take(MediumTree(Blue)).toOption.get
        )
        val johnForestBlock = Block(1, 1, 0, MediumTree(Blue))
        val forestBlocks = List(johnForestBlock)
        val playerBoards = List(johnsBoard)

        val expectedJohnBoardAfterPlace = johnsBoard.withdrawResource(MediumTree(Blue).growResource)
        val expectedForestBlock = johnForestBlock.copy(plantItem = CoolingDownLargeTree(Blue))
        initialState
          .copy(
            forestBlocks = forestBlocks,
            playerBoards = playerBoards)
          .grow(john, Location(1, 1, 0)) shouldBe Right(
            initialState
              .copy(
                forestBlocks = List(expectedForestBlock),
                playerBoards = List(expectedJohnBoardAfterPlace.toOption.get)
              )
        )
      }
      "should success if there is enough sun, have available bigger tree, also discard replaced tree if no available space" in {
        val john = Player("John", Blue)
        val johnsBoard = john.initBoard.copy(
          stock = List(LargeTree(Blue)),
          sun = 3,
          store = PlantStore(Blue)
        )
        val johnForestBlock = Block(1, 1, 0, MediumTree(Blue))
        val forestBlocks = List(johnForestBlock)
        val playerBoards = List(johnsBoard)

        val expectedJohnBoardAfterPlace = johnsBoard.withdrawResource(MediumTree(Blue).growResource)
        val expectedForestBlock = johnForestBlock.copy(plantItem = CoolingDownLargeTree(Blue))
        initialState
          .copy(
            forestBlocks = forestBlocks,
            playerBoards = playerBoards)
          .grow(john, Location(1, 1, 0)) shouldBe Right(
          initialState
            .copy(
              forestBlocks = List(expectedForestBlock),
              playerBoards = List(expectedJohnBoardAfterPlace.toOption.get)
            )
        )
      }
    }
    "buyItem" - {
      "should fail if the buying tree/seed is not the player's color" in {
        val john = Player("John", Green)
        initialState
          .buyItem(john, Seed(Blue)) shouldBe Left("Cannot buy different species")
      }
      "should fail if the player is not found on the board" in {
        val otherPlayer = Player("Jim", Blue)
        initialState
          .buyItem(otherPlayer, Seed(Blue)) shouldBe Left("Player not found")
      }
      "should fail if there is no available tree/seed in the player board" in {
        val john = Player("John", Green)
        val johnBoard = john.initBoard
        val johnEmptySeedInStoreBoard =
          johnBoard.copy(store =
            johnBoard.store
              .take(Seed(Green))
              .flatMap(_.take(Seed(Green)))
              .flatMap(_.take(Seed(Green)))
              .flatMap(_.take(Seed(Green))).toOption.get)
        initialState
          .copy(playerBoards = List(johnEmptySeedInStoreBoard))
          .buyItem(john, Seed(Green)) shouldBe Left("Out of stock")
      }
      "should fail if there is available tree/seed in the player board but not enough sun" in {
        val john = Player("John", Green)
        val johnBoard = john.initBoard
        initialState
          .copy(playerBoards = List(johnBoard))
          .buyItem(john, Seed(Green)) shouldBe Left("Not enough sun")
      }
      "should deduct sun, place bought item in stock, and deduct item from the store if there is enough sun, and available tree in the player's board" in {
        val john = Player("John", Green)
        val johnBoard = john.initBoard.copy(sun = 9)
        val expectedJohnBoard = johnBoard
            .copy(
              sun = 9 - Seed(Green).growCost,
              stock = johnBoard.stock :+ Seed(Green),
              store = johnBoard.store.take(Seed(Green)).toOption.get)
        initialState
          .copy(playerBoards = List(johnBoard))
          .buyItem(john, Seed(Green)) shouldBe Right(initialState.copy(playerBoards = List(expectedJohnBoard))
        )
      }
    }
    "smartMove" - {
      "should fail if there is no available tree/seed in stock, and no available tree/seed in player board" is pending
      "should fail if there is no available tree/seed in stock, and not enough sun to buy" is pending
      "should fail if there is no available tree/seed in stock, enough sun to buy, available tree/seed in player board but not enough sun to upgrade" is pending
      "should success and discard the replaced tree/seed if there is no available tree/seed in stock, enough sun to buy, available tree/seed in player board, and enough sun to upgrade" is pending
      "should success and place back tree/seed in to the top most available player board space if there is no available tree/seed in stock, enough sun to buy, available tree/seed in player board, and enought sun to upgrade" is pending
      "should fail if there is available tree/seed in stock but not enough sun to upgrade" is pending
      "should success and discard the replaced tree/seed if there is no available space in player board" is pending
      "should success and place back tree/seed in to the top most available player board space if available" is pending
    }
  }
}

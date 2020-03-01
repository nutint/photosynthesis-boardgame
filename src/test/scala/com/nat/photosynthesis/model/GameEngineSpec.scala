package com.nat.photosynthesis.model

import com.nat.photosynthesis.model
import org.scalatest.{Assertion, FreeSpec, Matchers}

class GameEngineSpec extends FreeSpec with Matchers
{


  val johnAndRose =
    Player(name = "John", plantType = Green) ::
    Player(name = "Rose", plantType = Yellow) :: Nil

  "GameEngineRegistrationState" - {
    val emptyGameEngineState = GameEngineRegistrationState(players = Nil)
    val onlyJohnGameEngineState = GameEngineRegistrationState(
      players = Player(
        name = "John",
        plantType = Green
      ) :: Nil
    )

    val johnWithRoseGameEngine = GameEngineRegistrationState(
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
        val tokenStock = TokenStock(Nil, Nil, Nil, List(TokenTierFour(1)))
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
          GameEnginePlacingFirst2TreesState(
            plantingTreePlayer = 0,
            playerBoards = johnWithRoseGameEngine.players.map(_.initBoard),
            forestBlocks = Nil,
            tokenStock = TokenStock(Nil, Nil, Nil, Nil)
          )
        )
      }

      "should transfer TokenStock from Registration state" in {
        val tokenStock = TokenStock(Nil, Nil, Nil, List(TokenTierFour(12)))
        johnWithRoseGameEngine
          .setTokenStock(tokenStock).startGame shouldBe Right(
          GameEnginePlacingFirst2TreesState(
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

    val nonPlayerPlaceTreeYet = GameEnginePlacingFirst2TreesState(
      plantingTreePlayer = 0,
      playerBoards = johnAndRose.map(_.initBoard),
      forestBlocks = Nil,
      tokenStock = TokenStock()
    )
    "placeTree" - {
      val boardLocation = BoardLocation(0, 3, 3)
      val smallTree = SmallTree(Green)

      def verifySuccessAttributes(currentPlayerPos: Int, actionPlayer: Int, assertFn: GameEnginePlacingFirst2TreesState => Assertion): Assertion = {
        nonPlayerPlaceTreeYet
          .copy(plantingTreePlayer = currentPlayerPos)
          .placeTree(actionPlayer, boardLocation, smallTree) match {
          case Right(ns) => ns match {
            case gs: GameEnginePlacingFirst2TreesState => assertFn(gs)
            case _ => assert(false)
          }
          case _ => assert(false)
        }
      }

      "active player do the place" - {

        "should have new location available in forestBlock" in {
          verifySuccessAttributes(0, 0, _.forestBlocks should contain(boardLocation.toForestBlock(smallTree)))
        }

        "should move to next player" in {
          verifySuccessAttributes(0, 0, _.plantingTreePlayer shouldBe 1)
        }

        "should move to first player if the current player is the last one" in {
          verifySuccessAttributes(1, 1, _.plantingTreePlayer shouldBe 0)
        }

      }

      "should move to the next player after a player place" is pending
      "should not allow non-active player to plant the tree" is pending
      "should not allow place any plant on the non-edge location" is pending
    }

    "startPlaying" - {
      "should allow start playing if all players placed 2 trees" is pending
      "should not allow start playing if all player are not finished place 2 trees yet" is pending
    }
  }
}

package com.nat.photosynthesis.model

import org.scalatest.{FreeSpec, Matchers}

class GameEngineSpec extends FreeSpec with Matchers
{
  "should pass" in {
    true shouldBe true
  }

  "GameEngineRegistrationState" - {
    val emptyGameEngineState = GameEngineRegistrationState(players = Nil)
    val onlyJohnGameEngineState = GameEngineRegistrationState(
      players = Player(
        name = "John",
        plantType = Green
      ) :: Nil
    )
    val johnWithRoseGameEngine = GameEngineRegistrationState(
      players = Player(
        name = "John",
        plantType = Green
      ) :: Player(
        name = "Rose",
        plantType = Yellow
      ) :: Nil
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

    "startGame" - {
      "should not be able to start when there is only 1 player" in {
        onlyJohnGameEngineState.startGame shouldBe Left("Cannot start game less than 2 players")
      }

      "should be able to start the game when there is 2 player" in {
        johnWithRoseGameEngine.startGame shouldBe Right(
          GameEngineSetupState(
            playerBoards = johnWithRoseGameEngine.players.map(_.initBoard),
            forestBlocks = Nil,
            remainingTokens = TokenStock(Nil, Nil, Nil, Nil)
          ))
      }
    }
  }
}

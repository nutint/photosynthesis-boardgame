package com.nat.photosynthesis.service

import com.nat.photosynthesis.repository.GameEngineRepository
import com.nat.photosynthesis.service.model.{GameEngine, GameOver, Green}
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._


class GameEngineCommandHandlerSpec extends FreeSpec with Matchers with MockitoSugar {

  "processCommand" - {

    "AddGame" - {
      "should be able to create room" in {
        val gameEngineRepository = mock[GameEngineRepository]
        val gameEngineCommandHandler = new GameEngineCommandHandler(gameEngineRepository)

        gameEngineCommandHandler.processCommand(AddGame) match {
          case Right(GameAdded(_)) => assert(true)
          case _ => assert(false)
        }
      }
    }

    "AddPlayer" - {

      "should fail if the specified room id is not found" in {
        val gameEngineRepository = mock[GameEngineRepository]
        val gameEngineCommandHandler = new GameEngineCommandHandler(gameEngineRepository)

        when(gameEngineRepository.getGameById("nonExistedId"))
          .thenReturn(Left("Not Found"))

        gameEngineCommandHandler.processCommand(AddPlayer("nonExistedId", "John", Green)) match {
          case Right(AddPlayerFailed(reason)) => reason shouldBe "Not Found"
          case _ => assert(false)
        }
      }

      "should fail if the specified room is non in the registration state" in {
        val gameEngineRepository = mock[GameEngineRepository]
        val gameEngineCommandHandler = new GameEngineCommandHandler(gameEngineRepository)

        when(gameEngineRepository.getGameById("nonExistedId"))
          .thenReturn(Right(GameOver(Nil, Nil)))

        gameEngineCommandHandler.processCommand(AddPlayer("nonExistedId", "John", Green)) match {
          case Right(AddPlayerFailed(reason)) => reason shouldBe "Unable to add player at this time"
          case _ => assert(false)
        }
      }

      "should be able to add player to an existing registered room" in {
        val gameEngineRepository = mock[GameEngineRepository]
        val gameEngineCommandHandler = new GameEngineCommandHandler(gameEngineRepository)

        when(gameEngineRepository.getGameById("anId"))
          .thenReturn(Right(GameEngine()))

        gameEngineCommandHandler.processCommand(AddPlayer("anId", "John", Green)) match {
          case Right(PlayerAdded) => assert(true)
          case _ => assert(false)
        }
      }
    }

  }
}

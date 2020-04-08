package com.nat.photosynthesis.service

import com.nat.photosynthesis.repository.GameEngineRepository
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.mockito.MockitoSugar


class GameEngineCommandHandlerSpec extends FreeSpec with Matchers with MockitoSugar {

  "processCommand" - {

    "CreateRoom" - {
      "should be able to create room" in {
        val mockedGameEngineStorage = mock[GameEngineRepository]
        val gameEngineCommandHandler = new GameEngineCommandHandler(mockedGameEngineStorage)
        gameEngineCommandHandler.processCommand(AddGame) match {
          case Right(GameAdded(_)) => assert(true)
          case _ => assert(false)
        }
      }
    }

  }
}

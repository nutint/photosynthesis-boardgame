package com.nat.photosynthesis.service

import com.nat.model.Identifiable
import com.nat.photosynthesis.service.model.engine.{GameEngine, Registration}
import com.nat.photosynthesis.service.model.{AddPlayer, Green, Player, Processor, ScoringTokenStacks, engine}
import com.nat.photosynthesis.service.repository.GameRepository
import com.nat.photosynthesis.utils.UUIDGenerator
import org.scalatest.{AsyncFreeSpec, BeforeAndAfterEach, Matchers}
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._

import scala.concurrent.Await
import scala.concurrent.duration._

class GameServiceSpec
  extends AsyncFreeSpec
    with MockitoSugar
    with Matchers
    with BeforeAndAfterEach
{
  import Processor._

  val mockedUUIDGenerator = mock[UUIDGenerator]
  private val gameId = "69e304e4-1716-4255-abbf-bbe04a29e7fe"

  override def beforeEach() = {
    reset(mockedUUIDGenerator)
    when(mockedUUIDGenerator.generate()).thenReturn(gameId)
  }

  "createGame" - {
    "should be able to create game when no error" in {
      val initialGameEngine = Registration(Nil, ScoringTokenStacks())
      val john = Player("John", Green)
      val newGameEngineWithNoID = initialGameEngine
        .processCommand(AddPlayer(john))

      val gameService = new GameService(mock[GameRepository], mockedUUIDGenerator)

      gameService.createGame(john:: Nil).map(_.map(_.value) shouldEqual newGameEngineWithNoID)
    }

    "should use generated game id from UUIDGenerator" in {
      val john = Player("John", Green)

      val gameService = new GameService(mock[GameRepository], mockedUUIDGenerator)

      val expectedGameId: Either[String, String] = Right(gameId)

      val newGame = Await.result(gameService.createGame(john:: Nil), 5 second)
      newGame.map(_.id) shouldEqual expectedGameId
    }

    "should use UUIDGenerator once" in {
      val john = Player("John", Green)

      val gameService = new GameService(mock[GameRepository], mockedUUIDGenerator)

      Await.result(gameService.createGame(john:: Nil), 5 second)
      verify(mockedUUIDGenerator, times(1)) generate() shouldEqual null
    }

    "violate rules" is pending
  }

}


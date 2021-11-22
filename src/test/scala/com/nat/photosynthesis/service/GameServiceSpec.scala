package com.nat.photosynthesis.service

import com.nat.model.Identifiable
import com.nat.photosynthesis.service.model.engine.{GameEngine, Registration}
import com.nat.photosynthesis.service.model.{AddPlayer, Green, Player, Processor, ScoringTokenStacks, engine}
import com.nat.photosynthesis.service.repository.GameRepository
import com.nat.photosynthesis.utils.UUIDGenerator
import org.scalatest.{AsyncFreeSpec, FreeSpec, Matchers}
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._

class GameServiceSpec
  extends AsyncFreeSpec
    with MockitoSugar
    with Matchers
{
  import Processor._

  val mockedUUIDGenerator = mock[UUIDGenerator]
  private val gameId = "69e304e4-1716-4255-abbf-bbe04a29e7fe"
  when(mockedUUIDGenerator.generate()).thenReturn(gameId)

  "createGame" - {
    "should be able to create game when no error" in {
      val initialGameEngine = Registration(Nil, ScoringTokenStacks())
      val john = Player("John", Green)
      val expectedGameEngine: Either[String, Identifiable[GameEngine]] =
        initialGameEngine
          .processCommand(AddPlayer(john))
          .map(x => Identifiable(gameId, x))

      val gameService = new GameService(mock[GameRepository], mockedUUIDGenerator)

      // M(x).map(x -> y): M(y)
      gameService.createGame(john:: Nil).map({
        case Right(newGameEngine) =>
          expectedGameEngine
            .map(x => x shouldEqual newGameEngine)
            .getOrElse(assert(false))
        case _ => assert(false)
      })
    }
//    "should be able to add player when the room is empty"
  }

}


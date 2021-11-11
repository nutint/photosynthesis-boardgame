package com.nat.photosynthesis.service

import com.nat.model.Identifiable
import com.nat.photosynthesis.service.model.engine.{GameEngine, Registration}
import com.nat.photosynthesis.service.model.{AddPlayer, Green, Player, Processor, ScoringTokenStacks, engine}
import com.nat.photosynthesis.service.repository.GameRepository
import org.scalatest.{AsyncFreeSpec, FreeSpec, Matchers}
import org.scalatest.mockito.MockitoSugar

class GameServiceSpec
  extends AsyncFreeSpec
    with MockitoSugar
    with Matchers
{
  import Processor._

  "createGame" - {
    "should be able to create game when no error" in {
      val initialGameEngine = Registration(Nil, ScoringTokenStacks())
      val john = Player("John", Green)
      val expectedGameEngine: Either[String, Identifiable[GameEngine]] = initialGameEngine.processCommand(AddPlayer(john)).map(x => Identifiable("fakeId", x))

      val gameService = new GameService(mock[GameRepository])

      // M(x).map(x -> y): M(y)
      gameService.createGame(john:: Nil).map({
        case Right(newGameEngine) => expectedGameEngine.map(x => x shouldEqual newGameEngine).getOrElse(assert(false))
        case _ => assert(false)
      })
    }
//    "should be able to add player when the room is empty"
  }

}
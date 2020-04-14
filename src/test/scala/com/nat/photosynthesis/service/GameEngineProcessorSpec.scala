package com.nat.photosynthesis.service

import com.nat.photosynthesis.service.model.{engine, _}
import com.nat.photosynthesis.service.model.engine.{GameOver, Registration}
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FreeSpec, Matchers}

class GameEngineProcessorSpec
  extends FreeSpec
    with Matchers
    with MockitoSugar
{

  import Processor._
  "processCmd" - {

    "AddPlayer" - {

      "Registration state" - {

        "should be able to add player when the room is empty" in {
          val gameEngine = Registration(Nil, ScoringTokenStacks())
          val john = Player("John", Green)
          gameEngine.processCommand(AddPlayer(john)) shouldBe Right(engine.Registration(john :: Nil, ScoringTokenStacks()))
        }

        "should not be able to add player if the plant type is already have" in {
          val john = Player("John", Green)
          val gameEngine = engine.Registration(john :: Nil, ScoringTokenStacks())
          val mary = Player("Mary", Green)
          gameEngine.processCommand(AddPlayer(mary)) shouldBe Left("Unable to add player with same plant type")
        }

        "should not be able to add player if the name is duplicate" in {
          val john = Player("John", Green)
          val gameEngine = engine.Registration(john :: Nil, ScoringTokenStacks())
          val anotherJohn = Player("John", Blue)
          gameEngine.processCommand(AddPlayer(anotherJohn)) shouldBe Left("Unable to add player with the same name")
        }
      }

      "Other State" - {
        "should not be able to add player in other state" in {
          val gameEngine = GameOver(Nil, Nil)
          gameEngine.processCommand(AddPlayer(Player("John", Green))) shouldBe Left("Bad command")
        }
      }
    }

    "StartGame" - {
      "Registration state" - {

        "should be able to start state if the player is more than 2" in {
          val gameEngine = Registration(Player("John", Green) :: Player("Mary", Blue) :: Nil, ScoringTokenStacks())
          gameEngine.processCommand(StartGame) match {
            case Right(_) => assert(true)
            case _ => assert(false)
          }
        }

        "should not be able to start game if the player is not more than 2" in {
          val gameEngine = Registration(Player("John", Green) :: Nil, ScoringTokenStacks())
          gameEngine.processCommand(StartGame) match {
            case Left(_) => assert(true)
            case _ => assert(false)
          }
        }
      }
    }
  }

  "processCommands" - {
    "should be able to process the following commands" in {
      val commands =
        AddPlayer(Player("John", Green)) ::
        AddPlayer(Player("Mary", Blue)) ::
        AddPlayer(Player("Madonna", Yellow)) ::
        StartGame :: Nil

      val registration = Registration(Nil, ScoringTokenStacks())
      registration.processCommands(commands) match {
        case Right(_) => assert(true)
        case _ => assert(false)
      }
    }
  }
}

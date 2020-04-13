package com.nat.photosynthesis.service.model

import org.scalatest.{FreeSpec, Matchers}

class ScoreEngineSpec extends FreeSpec with Matchers {

  "calculatePlayerScore" - {
    val john = Player("John", Green)

    "should return 0 when there is no block on the board" in {
      val forestBlocks = Nil
      ScoreEngine.calculatePlayerScore(john, forestBlocks, SunLocation0) shouldBe 0
    }

    "should return 0 if there is 1 seed in the board and its the same color" in {
      val forestBlock = List(Block(Location(0, 3, 3), Seed(Green)))
      ScoreEngine.calculatePlayerScore(john, forestBlock, SunLocation0) shouldBe 0
    }

    "should return 1 if there is 1 small tree in the board and its the same color" in {
      val forestBlock = List(Block(Location(0, 3, 3), SmallTree(Green)))
      ScoreEngine.calculatePlayerScore(john, forestBlock, SunLocation0) shouldBe 1
    }

    "should return 2 if there is 1 medium tree in the board and its the same color" in {
      val forestBlock = List(Block(Location(0, 3, 3), MediumTree(Green)))
      ScoreEngine.calculatePlayerScore(john, forestBlock, SunLocation0) shouldBe 2
    }

    "should return 3 if there is 1 large tree in the board and its the same color" in {
      val forestBlock = List(Block(Location(0, 3, 3), LargeTree(Green)))
      ScoreEngine.calculatePlayerScore(john, forestBlock, SunLocation0) shouldBe 3
    }
  }
}

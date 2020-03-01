package com.nat.photosynthesis.model

import org.scalatest.{FreeSpec, Matchers}

class ScoreEngineSpec extends FreeSpec with Matchers {

  "calculatePlayerScore" - {
    val john = Player("John", Green)

    "should return 0 when there is no block on the board" in {
      val forestBlocks = Nil
      ScoreEngine.calculatePlayerScore(john, forestBlocks, SunLocation0) shouldBe 0
    }

    "should return 1 if there is 1 small tree in the board and its the same color" in {
      val forestBlock = List(ForestBlock(BoardLocation(0, 3, 3), SmallTree(Green)))
      ScoreEngine.calculatePlayerScore(john, forestBlock, SunLocation0) shouldBe 1
    }
  }
}

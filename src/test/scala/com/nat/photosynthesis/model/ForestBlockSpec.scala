package com.nat.photosynthesis.model

import org.scalatest.{FreeSpec, Matchers}

class ForestBlockSpec extends FreeSpec with Matchers {

  "calculateScore" - {

    "should return 1 when there is only 1 tree on the list" in {
      val block = ForestBlock(BoardLocation(3, 3, 0), SmallTree(Green))
      block.calculateScore(SunLocation0, block :: Nil) shouldBe 1
    }
  }
}

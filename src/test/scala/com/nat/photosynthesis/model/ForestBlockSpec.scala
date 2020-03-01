package com.nat.photosynthesis.model

import org.scalatest.{FreeSpec, Matchers}

class ForestBlockSpec extends FreeSpec with Matchers {

  "calculateScore" - {
    val green1 = ForestBlock(BoardLocation(0, 3, 3), SmallTree(Green))
    val green2 = ForestBlock(BoardLocation(-1, 2, 3), SmallTree(Green))

    "should return 1 when there is only 1 tree on the list" in {
      green1.calculateScore(SunLocation0, green1 :: Nil) shouldBe 1
    }

    "should return 2 when there is more than 2 tree with no shadow on the list" in {
      val blocks = green1 :: green2 :: Nil
      green1.calculateScore(SunLocation0, blocks) shouldBe 1
      green2.calculateScore(SunLocation0, blocks) shouldBe 1
    }
  }
}

package com.nat.photosynthesis.model

import org.scalatest.{FreeSpec, Matchers}

class BoardLocationSpec extends FreeSpec with Matchers {

  "withTree" - {
    "should work correctly" in {
      val location = BoardLocation(0, 0, 0)
      location.toForestBlock(SmallTree(Green)) shouldBe ForestBlock(location, SmallTree(Green))
    }
  }
}

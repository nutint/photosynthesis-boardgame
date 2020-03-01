package com.nat.photosynthesis.model

import org.scalatest.{FreeSpec, Matchers}

class PlantItemSpec extends FreeSpec with Matchers {

  "Seed" - {
    "should have score as 0" in {
      Seed(Green).score shouldBe 0
    }
  }

  "SmallTree" - {
    "should have score as 1" in {
      SmallTree(Green).score shouldBe 1
    }
  }


  "MediumTree" - {
    "should have score as 2" in {
      MediumTree(Green).score shouldBe 2
    }
  }

  "LargeTree" - {
    "should have score as 3" in {
      LargeTree(Green).score shouldBe 3
    }
  }
}

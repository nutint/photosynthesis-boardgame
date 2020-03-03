package com.nat.photosynthesis.model

import org.scalatest.{FreeSpec, Matchers}

class PlantItemSpec extends FreeSpec with Matchers {

  "Seed" - {
    "should have score as 0" in {
      Seed(Green).score shouldBe 0
    }

    "should have height as 0" in {
      Seed(Green).height shouldBe 0
    }
  }

  "SmallTree" - {
    "should have score as 1" in {
      SmallTree(Green).score shouldBe 1
    }

    "should have height as 0" in {
      SmallTree(Green).height shouldBe 1
    }
  }


  "MediumTree" - {
    "should have score as 2" in {
      MediumTree(Green).score shouldBe 2
    }

    "should have height as 0" in {
      MediumTree(Green).height shouldBe 2
    }
  }

  "LargeTree" - {
    "should have score as 3" in {
      LargeTree(Green).score shouldBe 3
    }

    "should have height as 0" in {
      LargeTree(Green).height shouldBe 3
    }
  }

  "grow" - {
    "should be cooled down after grow" in {
      Seed(Green).grow shouldBe CooledDownSmallTree(Green)
      Seed(Green).growCost shouldBe 1
      SmallTree(Green).grow shouldBe CooledDownMediumTree(Green)
      SmallTree(Green).growCost shouldBe 2
      MediumTree(Green).grow shouldBe CooledDownLargeTree(Green)
      MediumTree(Green).growCost shouldBe 3
    }
  }

  "seed" - {
    "should be cooled down after seed" in {
      SmallTree(Green).seed shouldBe CooledDownSmallTree(Green)
      SmallTree(Green).seedCost shouldBe 1
      MediumTree(Green).seed shouldBe CooledDownMediumTree(Green)
      MediumTree(Green).seedCost shouldBe 1
      LargeTree(Green).seed shouldBe CooledDownLargeTree(Green)
      LargeTree(Green).seedCost shouldBe 1
    }
  }

  "reset" - {
    "should be reset to normal" in {
      CooledDownSmallTree(Green).reset shouldBe SmallTree(Green)
      CooledDownMediumTree(Green).reset shouldBe MediumTree(Green)
      CooledDownLargeTree(Green).reset shouldBe LargeTree(Green)
    }
  }
}

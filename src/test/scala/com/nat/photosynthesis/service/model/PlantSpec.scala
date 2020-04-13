package com.nat.photosynthesis.service.model

import org.scalatest.{FreeSpec, Matchers}

class PlantSpec extends FreeSpec with Matchers {

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
      Seed(Green).grow shouldBe CoolingDownSmallTree(Green)
      Seed(Green).growCost shouldBe 1
      SmallTree(Green).grow shouldBe CoolingDownMediumTree(Green)
      SmallTree(Green).growCost shouldBe 2
      MediumTree(Green).grow shouldBe CoolingDownLargeTree(Green)
      MediumTree(Green).growCost shouldBe 3
    }
  }

  "seed" - {
    "should be cooled down after seed" in {
      import Plant._
      SmallTree(Green).seed shouldBe CoolingDownSmallTree(Green)
      SmallTree(Green).seedCost shouldBe Resource(Seed(Green),1)
      MediumTree(Green).seed shouldBe CoolingDownMediumTree(Green)
      MediumTree(Green).seedCost shouldBe Resource(Seed(Green),1)
      LargeTree(Green).seed shouldBe CoolingDownLargeTree(Green)
      LargeTree(Green).seedCost shouldBe Resource(Seed(Green),1)
    }
  }

  "reset" - {
    "should be reset to normal" in {
      CoolingDownSmallTree(Green).reset shouldBe SmallTree(Green)
      CoolingDownMediumTree(Green).reset shouldBe MediumTree(Green)
      CoolingDownLargeTree(Green).reset shouldBe LargeTree(Green)
    }
  }

  "growResource" - {
    import Plant.Resource
    "should returns correct upgrade resource" in {
      Seed(Green).growResource shouldBe Resource(SmallTree(Green), 1)
      SmallTree(Green).growResource shouldBe Resource(MediumTree(Green), 2)
      MediumTree(Green).growResource shouldBe Resource(LargeTree(Green), 3)
    }
  }
}

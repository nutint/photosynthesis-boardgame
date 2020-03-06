package com.nat.photosynthesis.model

import org.scalatest.{FreeSpec, Matchers}

class PlantStoreSpec extends FreeSpec with Matchers {

  "PlantStore.apply" - {
    "should create correctly" in {
      PlantStore(Green) shouldBe PlantStore(
        plantType = Green,
        seedStore = StoreSpace[Seed](List(1, 1, 2, 2)),
        smallTreeStore = StoreSpace[SmallTree](List(2, 2, 3, 3)),
        mediumTreeStore = StoreSpace[MediumTree](List(3, 3, 4, 4)),
        largeTreeStore = StoreSpace[LargeTree](List(4, 5))
      )
    }
  }
}

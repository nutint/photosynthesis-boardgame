package com.nat.photosynthesis.service.model

import org.scalatest.{FreeSpec, Matchers}

class PlantStoreSpec extends FreeSpec with Matchers {

  private val seedStoreSpace: StoreSpace[Seed] = StoreSpace[Seed](List(1, 1, 2, 2))
  private val smallTreeStoreSpace: StoreSpace[SmallTree] = StoreSpace[SmallTree](List(2, 2, 3, 3))
  private val mediumTreeStoreSpace: StoreSpace[MediumTree] = StoreSpace[MediumTree](List(3, 3, 4, 4))
  private val largeTreeStoreSpace: StoreSpace[LargeTree] = StoreSpace[LargeTree](List(4, 5))
  private val originalPlantStore = PlantStore(
    plantType = Green,
    seedStore = seedStoreSpace,
    smallTreeStore = smallTreeStoreSpace,
    mediumTreeStore = mediumTreeStoreSpace,
    largeTreeStore = largeTreeStoreSpace
  )

  "PlantStore.apply" - {
    "should create correctly" in {
      PlantStore(Green) shouldBe originalPlantStore
    }
  }

  "getPrice" - {
    "should work correctly" in {
      originalPlantStore.getPrice(Seed(Green)) shouldBe seedStoreSpace.currentPrice
      originalPlantStore.getPrice(SmallTree(Green)) shouldBe smallTreeStoreSpace.currentPrice
      originalPlantStore.getPrice(MediumTree(Green)) shouldBe mediumTreeStoreSpace.currentPrice
      originalPlantStore.getPrice(LargeTree(Green)) shouldBe largeTreeStoreSpace.currentPrice
      originalPlantStore.getPrice(CoolingDownLargeTree(Green)) shouldBe Left("Invalid Plant Type")
    }
  }

  "take" - {
    "should work correctly" in {
      originalPlantStore.take(Seed(Green)) shouldBe seedStoreSpace.take.map(ss => originalPlantStore.copy(seedStore = ss))
      originalPlantStore.take(SmallTree(Green)) shouldBe smallTreeStoreSpace.take.map(ss => originalPlantStore.copy(smallTreeStore = ss))
      originalPlantStore.take(MediumTree(Green)) shouldBe mediumTreeStoreSpace.take.map(ss => originalPlantStore.copy(mediumTreeStore = ss))
      originalPlantStore.take(LargeTree(Green)) shouldBe largeTreeStoreSpace.take.map(ss => originalPlantStore.copy(largeTreeStore = ss))
      originalPlantStore.take(CoolingDownLargeTree(Green)) shouldBe Left("Invalid Plant Type")
    }
  }

  "putBack" - {
    "should work correctly" in {
      originalPlantStore.putBack(Seed(Green)) shouldBe seedStoreSpace.putBack.map(ss => originalPlantStore.copy(seedStore = ss))
      originalPlantStore.putBack(SmallTree(Green)) shouldBe smallTreeStoreSpace.putBack.map(ss => originalPlantStore.copy(smallTreeStore = ss))
      originalPlantStore.putBack(MediumTree(Green)) shouldBe mediumTreeStoreSpace.putBack.map(ss => originalPlantStore.copy(mediumTreeStore = ss))
      originalPlantStore.putBack(LargeTree(Green)) shouldBe largeTreeStoreSpace.putBack.map(ss => originalPlantStore.copy(largeTreeStore = ss))
      originalPlantStore.putBack(CoolingDownLargeTree(Green)) shouldBe Left("Invalid Plant Type")
    }
  }
}

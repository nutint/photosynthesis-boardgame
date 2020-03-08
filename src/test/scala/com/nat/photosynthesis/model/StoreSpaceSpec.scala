package com.nat.photosynthesis.model

import org.scalatest.{FreeSpec, Matchers}

class StoreSpaceSpec extends FreeSpec with Matchers {

  "apply" - {
    "should create correctly" in {
      StoreSpace[Seed](List(1, 1, 2, 2)) shouldBe StoreSpace[Seed](List(1, 1, 2, 2), 0)
    }
  }

  "currentPrice" - {
    val storeSpace = StoreSpace[Seed](List(1, 1, 2, 2))
    "should return correct price" in {
      storeSpace.currentPrice shouldBe Right(1)
    }

    "should fail if there is no available item in the space" in {
      storeSpace
        .copy(currItem = 4)
        .currentPrice shouldBe Left("Out of stock")
    }
  }

  val storeSpace = StoreSpace[Seed](List(1, 2, 3, 4))

  "take" - {
    "should increase the price" in {
      val took1 = storeSpace
        .take

      took1.flatMap(_.currentPrice) shouldBe Right(2)

      took1
        .flatMap(_.take)
        .flatMap(_.currentPrice) shouldBe Right(3)
    }
    "should error if all are taken" in {
      storeSpace.copy(currItem = 4)
        .take shouldBe Left("Item not available")
    }
  }

  "putBack" - {
    "should decrease the price" in {
      storeSpace.copy(currItem = 1)
        .putBack shouldBe Right(storeSpace.copy(currItem = 0))
    }

    "should fail if the store is full" in {
      storeSpace.copy(currItem = 0)
        .putBack shouldBe Left("Already full")
    }
  }
}

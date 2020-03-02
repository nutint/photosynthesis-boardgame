package com.nat.photosynthesis.model

import org.scalatest.prop.TableDrivenPropertyChecks._
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

  "isUnderShadowOf" - {
    val green1 = ForestBlock(BoardLocation(0, 3, 3), SmallTree(Green))
    val green2 = ForestBlock(BoardLocation(-1, 2, 3), SmallTree(Green))
    val green3 = ForestBlock(BoardLocation(0, 3, 3), SmallTree(Green))
    val green4 = ForestBlock(BoardLocation(0, 2, 2), MediumTree(Green))

    "should be false is not in the same line" in {
      green1.isUnderShadowOf(green2, SunLocation0) shouldBe Right(false)
    }

    "should be left if its the same location" in {
      green3.isUnderShadowOf(green1, SunLocation0) shouldBe Left("Same Location")
    }

    "should be false if the behind one is bigger than the rear" in {
      green4.isUnderShadowOf(green1, SunLocation0) shouldBe Right(false)
    }

    "experiment different examples" in {
      val examples =
        Table(
          ("firstPlant",                           "secondPlant",                         "expectedResult"),
          (ForestBlock(0, 1, 1, SmallTree(Green)), ForestBlock(0, 3, 3, SmallTree(Green)), Right(false)),
          (ForestBlock(0, 2, 2, SmallTree(Green)), ForestBlock(0, 3, 3, SmallTree(Green)), Right(true))
        )
      forAll(examples) { (firstPlant, secondPlant, expectedResult) =>
        firstPlant.isUnderShadowOf(secondPlant, SunLocation0) shouldBe expectedResult
      }
    }
  }
}

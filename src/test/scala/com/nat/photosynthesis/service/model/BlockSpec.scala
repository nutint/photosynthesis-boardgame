package com.nat.photosynthesis.service.model

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{FreeSpec, Matchers}

class BlockSpec extends FreeSpec with Matchers {

  "calculateScore" - {
    val green1 = Block(Location(0, 3, 3), SmallTree(Green))
    val green2 = Block(Location(-1, 2, 3), SmallTree(Green))

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
    val green1 = Block(Location(0, 3, 3), SmallTree(Green))
    val green2 = Block(Location(-1, 2, 3), SmallTree(Green))
    val green3 = Block(Location(0, 3, 3), SmallTree(Green))
    val green4 = Block(Location(0, 2, 2), MediumTree(Green))

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
          (Block(0, 1, 1, SmallTree(Green)), Block(0, 3, 3, SmallTree(Green)), Right(false)),
          (Block(0, 2, 2, SmallTree(Green)), Block(0, 3, 3, SmallTree(Green)), Right(true)),
          (Block(0, 2, 2, SmallTree(Green)), Block(0, 4, 4, SmallTree(Green)), Right(false)),
          (Block(0, 2, 2, MediumTree(Green)), Block(0, 4, 4, SmallTree(Green)), Right(false)),
          (Block(0, 0, 0, SmallTree(Green)), Block(0, 3, 3, LargeTree(Green)), Right(true)),
          (Block(0, -1, -1, SmallTree(Green)), Block(0, 3, 3, LargeTree(Green)), Right(false))
        )
      forAll(examples) { (firstPlant, secondPlant, expectedResult) =>
        firstPlant.isUnderShadowOf(secondPlant, SunLocation0) shouldBe expectedResult
      }
    }
  }

  "isOwnBy" - {
    "should be true if it the same plant type with players'" in {
      Block(0, 1, 1, SmallTree(Green)).isOwnedBy(Player("John", Green)) shouldBe true
    }
    "should be false if not the same plant type with players'" in {
      Block(0, 1, 1, SmallTree(Green)).isOwnedBy(Player("John", Blue)) shouldBe false
    }
  }
}

package com.nat.photosynthesis.model

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{FreeSpec, Matchers}

class BoardLocationSpec extends FreeSpec with Matchers {

  "withTree" - {
    "should work correctly" in {
      val location = BoardLocation(0, 0, 0)
      location.toForestBlock(SmallTree(Green)) shouldBe ForestBlock(location, SmallTree(Green))
    }
  }

  "isEdgeLocation" - {
    "should returns true when this is the edge location" in {
      val location = BoardLocation(3, 3, 0)
      location.isEdgeLocation shouldBe true
    }
    "should returns false if this is not the edge location" in {
      val location = BoardLocation(0, 0, 0)
      location.isEdgeLocation shouldBe false
    }
  }

  "getBoardLocationTier" - {

    "should returns BoardLocationTier1 when pass the following inputs" in {
      val examples =
        Table(
          "BoardLocation",
          BoardLocation(0, 3, 3),
          BoardLocation(-1, 2, 3),
          BoardLocation(-2, 1, 3),
          BoardLocation(-3, 0, 3),
          BoardLocation(-3, -1, 2),
          BoardLocation(-3, -2, 1),
          BoardLocation(-3, -3, 0)
        )
      forAll(examples) { boardLocation =>
        boardLocation.getBoardLocationTier shouldBe BoardLocationTier1
      }
    }


    "should returns BoardLocationTier2 when pass the following inputs" in {
      val examples =
        Table(
          "BoardLocation",
          BoardLocation(0, 2, 2),
          BoardLocation(-1, 1, 2),
          BoardLocation(-2, 0, 2)
        )
      forAll(examples) { boardLocation =>
        boardLocation.getBoardLocationTier shouldBe BoardLocationTier2
      }
    }


    "should returns BoardLocationTier3 when pass the following inputs" in {
      val examples =
        Table(
          "BoardLocation",
          BoardLocation(0, 1, 1),
          BoardLocation(-1, 0, 1),
          BoardLocation(-1, -1, 0)
        )
      forAll(examples) { boardLocation =>
        boardLocation.getBoardLocationTier shouldBe BoardLocationTier3
      }
    }

    "should returns BoardLocationTier4 when pass the following inputs" in {
      BoardLocation(0, 0, 0).getBoardLocationTier shouldBe BoardLocationTier4
    }

  }
}

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

  "Position Check" - {

    val location1 = BoardLocation(0, 0, 0)
    val location2 = BoardLocation(0, 1, 1)
    val location3 = BoardLocation(-1, 2, 3)

    "isBehind" - {

      "SunLocation0" - {
        "should be true if location1 isBehind location2" in {
          location1.isBehind(location2, SunLocation0) shouldBe true
        }
        "should be false if swap between location1 and location2" in {
          location2.isBehind(location1, SunLocation0) shouldBe false
        }
        "should be false if not in the same line" in {
          location1.isBehind(location3, SunLocation0) shouldBe false
        }
      }
      "SunLocation3" - {
        "should be true if location1 isBehind location2" in {
          location1.isBehind(location2, SunLocation3) shouldBe false
        }
        "should be false if swap between location1 and location2" in {
          location2.isBehind(location1, SunLocation3) shouldBe true
        }
        "should be false if not in the same line" in {
          location1.isBehind(location3, SunLocation3) shouldBe false
        }

      }
    }

    "isSameLine" in {
      location1.isSameLine(location2, SunLocation0) shouldBe true
      location2.isSameLine(location1, SunLocation0) shouldBe true
      location1.isSameLine(location3, SunLocation0) shouldBe false
      location1.isSameLine(location2, SunLocation3) shouldBe true
      location2.isSameLine(location1, SunLocation3) shouldBe true
      location1.isSameLine(location3, SunLocation3) shouldBe false
    }

  }

  "getDistance" - {

    "should works by the following examples" in {
      val examples =
        Table(
          ("firstLocation", "secondLocation", "sunLocation",  "expectedResult"),
          ((0, 3, 3),       (0, 3, 3),       SunLocation0,    Same),
          ((0, 3, 3),       (0, 2, 2),       SunLocation0,    Front(1)),
          ((0, 2, 2),       (0, 3, 3),       SunLocation0,    Rear(1)),
          ((0, 3, 3),       (0, 2, 2),       SunLocation3,    Rear(1)),
          ((0, 2, 2),       (0, 3, 3),       SunLocation3,    Front(1)),
          ((-1, 1, 2),      (1, 1, 0),       SunLocation1,    Front(2)),
          ((1, 1, 0),       (-1, 1, 2),      SunLocation1,    Rear(2)),
          ((-1, 1, 2),      (1, 1, 0),       SunLocation4,    Rear(2)),
          ((1, 1, 0),       (-1, 1, 2),      SunLocation4,    Front(2)),
          ((0, -3, -3),     (3, 0, -3),      SunLocation2,    Front(3)),
          ((3, 0, -3),      (0, -3, -3),     SunLocation2,    Rear(3)),
          ((0, -3, -3),     (3, 0, -3),      SunLocation5,    Rear(3)),
          ((3, 0, -3),      (0, -3, -3),     SunLocation5,    Front(3)),
        )

      forAll(examples) { (firstLocation, secondLocation, sunLocation, expectedResult) =>
        BoardLocation(firstLocation)
          .getDistance(BoardLocation(secondLocation), sunLocation) shouldBe expectedResult
      }
    }
  }

  "inRadius" - {
    "should work by the following examples" in {
      BoardLocation(0, 0, 0).inRadius(BoardLocation(0, 1, 1), 1) shouldBe true
      BoardLocation(0, 0, 0).inRadius(BoardLocation(0, 1, 1), 0) shouldBe false
      BoardLocation(0, 0, 0).inRadius(BoardLocation(-3, 0, 3), 0) shouldBe false
      BoardLocation(0, 0, 0).inRadius(BoardLocation(-3, 0, 3), 3) shouldBe true
    }
  }
}

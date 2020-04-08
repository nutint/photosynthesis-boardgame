package com.nat.photosynthesis.model

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{FreeSpec, Matchers}

class LocationSpec extends FreeSpec with Matchers {

  "withTree" - {
    "should work correctly" in {
      val location = Location(0, 0, 0)
      location.toForestBlock(SmallTree(Green)) shouldBe Block(location, SmallTree(Green))
    }
  }

  "isEdgeLocation" - {
    "should returns true when this is the edge location" in {
      val location = Location(3, 3, 0)
      location.isEdgeLocation shouldBe true
    }
    "should returns false if this is not the edge location" in {
      val location = Location(0, 0, 0)
      location.isEdgeLocation shouldBe false
    }
  }

  "getBoardLocationTier" - {

    "should returns BoardLocationTier1 when pass the following inputs" in {
      val examples =
        Table(
          "BoardLocation",
          Location(0, 3, 3),
          Location(-1, 2, 3),
          Location(-2, 1, 3),
          Location(-3, 0, 3),
          Location(-3, -1, 2),
          Location(-3, -2, 1),
          Location(-3, -3, 0)
        )
      forAll(examples) { boardLocation =>
        boardLocation.getBoardLocationTier shouldBe LocationTier1$
      }
    }


    "should returns BoardLocationTier2 when pass the following inputs" in {
      val examples =
        Table(
          "BoardLocation",
          Location(0, 2, 2),
          Location(-1, 1, 2),
          Location(-2, 0, 2)
        )
      forAll(examples) { boardLocation =>
        boardLocation.getBoardLocationTier shouldBe LocationTier2$
      }
    }


    "should returns BoardLocationTier3 when pass the following inputs" in {
      val examples =
        Table(
          "BoardLocation",
          Location(0, 1, 1),
          Location(-1, 0, 1),
          Location(-1, -1, 0)
        )
      forAll(examples) { boardLocation =>
        boardLocation.getBoardLocationTier shouldBe LocationTier3$
      }
    }

    "should returns BoardLocationTier4 when pass the following inputs" in {
      Location(0, 0, 0).getBoardLocationTier shouldBe LocationTier4$
    }

  }

  "Position Check" - {

    val location1 = Location(0, 0, 0)
    val location2 = Location(0, 1, 1)
    val location3 = Location(-1, 2, 3)

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
        Location(firstLocation)
          .getDistance(Location(secondLocation), sunLocation) shouldBe expectedResult
      }
    }
  }

  "inRadius" - {
    "should work by the following examples" in {
      Location(0, 0, 0).inRadius(Location(0, 1, 1), 1) shouldBe true
      Location(0, 0, 0).inRadius(Location(0, 1, 1), 0) shouldBe false
      Location(0, 0, 0).inRadius(Location(-3, 0, 3), 0) shouldBe false
      Location(0, 0, 0).inRadius(Location(-3, 0, 3), 3) shouldBe true
    }
  }
}

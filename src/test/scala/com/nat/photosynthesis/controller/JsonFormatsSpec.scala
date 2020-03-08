package com.nat.photosynthesis.controller

import org.scalatest.prop.TableDrivenPropertyChecks._
import com.nat.photosynthesis.model._
import org.scalatest.{FreeSpec, Matchers}

import scala.util.{Failure, Try}

class JsonFormatsSpec extends FreeSpec with Matchers {

  import JsonFormats._
  import spray.json._

  "PlantTypeFormat" - {
    "read" - {

      "should be able to decode the following examples" in {
        val examples =
          Table(
            ("string", "expectedPlantType"),
            ("green", Green),
            ("Green", Green),
            ("GrEen ", Green),
            ("Yellow", Yellow),
            ("Orange", Orange),
            ("Blue", Blue)
          )
        forAll(examples) { (jsonString, plantType) =>
          PlantTypeFormat.read(JsString(jsonString)) shouldBe plantType
        }
      }

      "should error when pass other string" in {
        Try(PlantTypeFormat.read(JsString("other string"))) match {
          case Failure(ex) => ex.getMessage shouldBe "invalid plant type value: expected (green, yellow, blue, orange)"
          case _ => assert(false)
        }
      }

      "should error when pass other json kind" in {
        verifyReadError[PlantType](JsNumber(1), "invalid plant type value: expected string")
      }
    }
    "write" - {
      "should be able to write decodable json" in {
        PlantTypeFormat.read(PlantTypeFormat.write(Green)) shouldBe Green
      }
    }
  }

  "boardLocationFormat" - {
    "read" - {

      "should return object when decode success" in {
        val sourceString = """{ "x": 1, "y": 2, "z": 3 }""".parseJson
        boardLocationFormat.read(sourceString) shouldBe BoardLocation(1, 2, 3)
      }
      "should throw error when decode fail" in {
        val sourceString = """{ "y": 2, "z": 3 }""".parseJson
        verifyReadError[BoardLocation](sourceString, "Object is missing required member 'x'")
      }
    }

    "write" - {
      "should write decodable json" in {
        val expectedBoardLocation = BoardLocation(1, 2, 3)
        boardLocationFormat.read(
          boardLocationFormat.write(expectedBoardLocation)
        ) shouldBe expectedBoardLocation
      }
    }
  }

  "playerFormat" - {
    "read" - {

      "should decode the following json correctly" in {
        val sourceJson = """{ "name": "John", "plantType": "green"}""".parseJson
        playerFormat.read(sourceJson) shouldBe Player("John", Green)
      }

      "should decode fail if input incorrect player field" in {
        val sourceJson = """{ "namee": "John", "plantType": "green"}""".parseJson
        verifyReadError[Player](sourceJson, "Object is missing required member 'name'")
      }

      "should fail if unable to decode plantType" in {
        val sourceJson = """{ "name": "John", "plantType": "greeeen"}""".parseJson
        verifyReadError[Player](sourceJson, "invalid plant type value: expected (green, yellow, blue, orange)")
      }
    }

    "write" - {
      "should be able to write decodable json" in {
        val expectedPlayer = Player("John", Green)
        playerFormat.read(
          playerFormat.write(expectedPlayer)
        ) shouldBe expectedPlayer
      }
    }

  }

  private def verifyReadError[A](sourceJson: JsValue, expectedError: String)(implicit jsonFormat: JsonFormat[A]): Any = {
    Try(jsonFormat.read(sourceJson)) match {
      case Failure(exception) => exception.getMessage shouldBe expectedError
      case _ => assert(false)
    }
  }

  "BoardLocationTierFormat" - {
    "read" - {
      "should be able to read the following json" in {
        val examples =
          Table(
            ("jsonString", "expectedTier"),
            ("tier1", BoardLocationTier1),
            ("tier1  ", BoardLocationTier1),
            ("tIer1", BoardLocationTier1),
            ("tier2", BoardLocationTier2),
            ("tier3", BoardLocationTier3),
            ("tier4", BoardLocationTier4)
          )

        forAll(examples) { (tier, expectedTier) => {
          BoardLocationTierFormat.read(JsString(tier)) shouldBe expectedTier
        }}
      }
      "should fail if the input json is not correct string value" in {
        verifyReadError[BoardLocationTier](JsString("abd"), "invalid board location tier value: expected (tier1, tier2, tier3, tier4)")
      }
      "should fail if the input json is not string" in {
        verifyReadError[BoardLocationTier](JsNumber(123), "invalid board location tier value: expected string")
      }
    }
    "write" - {
      "should write decodable json" in {
        BoardLocationTierFormat.read(
          BoardLocationTierFormat.write(BoardLocationTier1)
        ) shouldBe BoardLocationTier1
      }
    }
  }

  "DistanceFormat" - {
    "read" - {
      "should be able to read the following input" in {
        val examples =
          Table(
            ("jsString", "expected"),
            ("front 1", Front(1)),
            ("Front 1", Front(1)),
            ("Rear 1", Rear(1)),
            ("Same", Same),
            ("DifferentLine", DifferentLine)
          )
        forAll(examples) { (jsString, expected) =>
          DistanceFormat.read(JsString(jsString)) shouldBe expected
        }
      }

      "should error if input string is the different way around" in {
        val examples =
          Table(
            "value",
            "abd",
            "front",
            "front a b",
            "front a",
            "asdf asdf asdf"
          )
        forAll(examples) { strVal =>
          verifyReadError[Distance](JsString(strVal), "invalid distance value: expected (same, differentline, front x, rear x)")
        }
      }
    }
    "write" - {
      "should write decodable json" in {
        val examples =
          Table(
            "ValidValues",
            Front(1),
            Rear(1),
            Same,
            DifferentLine
          )
        forAll(examples) { example =>
          DistanceFormat.read(
            DistanceFormat.write(example)
          ) shouldBe example
        }
      }
    }
  }

  "PlantItemFormat" - {
    "read" - {
      "should be able to read the following json" in {
        val examples =
          Table(
            ("jsonString", "expected"),
            ("green seed", Seed(Green)),
            ("green cooleddown-small-tree", CooledDownSmallTree(Green)),
            ("green small-tree", SmallTree(Green)),
            ("Green cooleddown-medium-tree", CooledDownMediumTree(Green)),
            ("gReen medium-tree", MediumTree(Green)),
            ("green cooleddown-large-tree", CooledDownLargeTree(Green)),
            ("green large-tree", LargeTree(Green))
          )
        forAll(examples) { (jsString, expected) =>
          PlantItemFormat.read(JsString(jsString)) shouldBe expected
        }
      }

      "should fail if enter the following input" in {
        val examples =
          Table(
            "failExample",
            "green something",
            "yellow someting-else",
            "blue abcd eftg",
            "adsfasdf asdfsadf",
            "asdfsadf medium-tree"
          )
        forAll(examples) { failExample =>
          verifyReadError[PlantItem](JsString(failExample), "invalid plant item value: expected example 'green seed', 'yellow medium-tree', 'blue cooleddown-medium-tree")
        }
        verifyReadError[PlantItem](JsNumber(123), "invalid plant item value: expected example 'green seed', 'yellow medium-tree', 'blue cooleddown-medium-tree")
      }
    }
    "write" - {
      "should be able to write decodable json" in {
        val examples =
          Table(
            "example",
            Seed(Green),
            CooledDownSmallTree(Green),
            SmallTree(Green),
            CooledDownMediumTree(Green),
            MediumTree(Green),
            CooledDownLargeTree(Green),
            LargeTree(Green)
          )
        forAll(examples) { example =>
          PlantItemFormat.read(
            PlantItemFormat.write(example)
          ) shouldBe example
        }
      }
    }
  }

  "storeSpaceFormat" - {
    "read" - {
      "should be able to read the following input" in {
        val jsonString =
          """
            |{
            |  "prices": [1, 2, 3, 4],
            |  "currItem": 0
            |}
            |""".stripMargin.parseJson

        storeSpaceFormat.read(jsonString) shouldBe StoreSpace(List(1, 2, 3, 4), 0)
      }
    }
    "write" - {
      "should be able to write decodable json" in {
        val expectedStoreSpace = StoreSpace(List(1, 2, 3, 4), 0)
        storeSpaceFormat.read(
          storeSpaceFormat.write(expectedStoreSpace)
        ) shouldBe expectedStoreSpace
      }
    }
  }
}

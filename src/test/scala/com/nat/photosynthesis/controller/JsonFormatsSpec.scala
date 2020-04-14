package com.nat.photosynthesis.controller

import org.scalatest.prop.TableDrivenPropertyChecks._
import com.nat.photosynthesis.service.model._
import com.nat.photosynthesis.service.model.engine._
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
        locationFormat.read(sourceString) shouldBe Location(1, 2, 3)
      }
      "should throw error when decode fail" in {
        val sourceString = """{ "y": 2, "z": 3 }""".parseJson
        verifyReadError[Location](sourceString, "Object is missing required member 'x'")
      }
    }

    "write" - {
      "should write decodable json" in {
        val expectedBoardLocation = Location(1, 2, 3)
        locationFormat.read(
          locationFormat.write(expectedBoardLocation)
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

  "BoardLocationTierFormat" - {
    "read" - {
      "should be able to read the following json" in {
        val examples =
          Table(
            ("jsonString", "expectedTier"),
            ("tier1", LocationTier1$),
            ("tier1  ", LocationTier1$),
            ("tIer1", LocationTier1$),
            ("tier2", LocationTier2$),
            ("tier3", LocationTier3$),
            ("tier4", LocationTier4$)
          )

        forAll(examples) { (tier, expectedTier) => {
          LocationTierFormat.read(JsString(tier)) shouldBe expectedTier
        }}
      }
      "should fail if the input json is not correct string value" in {
        verifyReadError[LocationTier](JsString("abd"), "invalid board location tier value: expected (tier1, tier2, tier3, tier4)")
      }
      "should fail if the input json is not string" in {
        verifyReadError[LocationTier](JsNumber(123), "invalid board location tier value: expected string")
      }
    }
    "write" - {
      "should write decodable json" in {
        LocationTierFormat.read(
          LocationTierFormat.write(LocationTier1$)
        ) shouldBe LocationTier1$
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
          DisplacementFormat.read(JsString(jsString)) shouldBe expected
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
          verifyReadError[Displacement](JsString(strVal), "invalid distance value: expected (same, differentline, front x, rear x)")
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
          DisplacementFormat.read(
            DisplacementFormat.write(example)
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
            ("green cooleddown-small-tree", CoolingDownSmallTree(Green)),
            ("green small-tree", SmallTree(Green)),
            ("Green cooleddown-medium-tree", CoolingDownMediumTree(Green)),
            ("gReen medium-tree", MediumTree(Green)),
            ("green cooleddown-large-tree", CoolingDownLargeTree(Green)),
            ("green large-tree", LargeTree(Green))
          )
        forAll(examples) { (jsString, expected) =>
          PlantFormat.read(JsString(jsString)) shouldBe expected
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
          verifyReadError[Plant](JsString(failExample), "invalid plant item value: expected example 'green seed', 'yellow medium-tree', 'blue cooleddown-medium-tree")
        }
        verifyReadError[Plant](JsNumber(123), "invalid plant item value: expected example 'green seed', 'yellow medium-tree', 'blue cooleddown-medium-tree")
      }
    }
    "write" - {
      "should be able to write decodable json" in {
        val examples =
          Table(
            "example",
            Seed(Green),
            CoolingDownSmallTree(Green),
            SmallTree(Green),
            CoolingDownMediumTree(Green),
            MediumTree(Green),
            CoolingDownLargeTree(Green),
            LargeTree(Green)
          )
        forAll(examples) { example =>
          PlantFormat.read(
            PlantFormat.write(example)
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

        storeSpaceFormat[Seed].read(jsonString) shouldBe StoreSpace[Seed](List(1, 2, 3, 4), 0)
      }
    }
    "write" - {
      "should be able to write decodable json" in {
        val expectedStoreSpace = StoreSpace[Seed](List(1, 2, 3, 4), 0)
        storeSpaceFormat[Seed].read(
          storeSpaceFormat[Seed].write(expectedStoreSpace)
        ) shouldBe expectedStoreSpace
      }
    }
  }

  "plantStoreFormat" - {

    val seedStoreSpace = StoreSpace[Seed](List(1, 2, 3, 4), 0)
    val smallTreeStoreSpace = StoreSpace[SmallTree](List(1, 2, 3, 4), 0)
    val mediumTreeStoreSpace = StoreSpace[MediumTree](List(1, 2, 3, 4), 0)
    val largeTreeStoreSpace = StoreSpace[LargeTree](List(1, 2, 3, 4), 0)

    val expectedPlantStore = PlantStore(
      plantType = Green,
      seedStore = seedStoreSpace,
      smallTreeStore = smallTreeStoreSpace,
      mediumTreeStore = mediumTreeStoreSpace,
      largeTreeStore = largeTreeStoreSpace
    )
    "read" - {
      "should be able to read the following input" in {
        val jsonString =
          s"""
            |{
            |  "plantType": "green",
            |  "seedStore": ${storeSpaceFormat[Seed].write(seedStoreSpace)},
            |  "smallTreeStore": ${storeSpaceFormat[SmallTree].write(smallTreeStoreSpace)},
            |  "mediumTreeStore": ${storeSpaceFormat[MediumTree].write(mediumTreeStoreSpace)},
            |  "largeTreeStore": ${storeSpaceFormat[LargeTree].write(largeTreeStoreSpace)}
            |}
            |""".stripMargin.parseJson

        plantStoreFormat.read(jsonString) shouldBe expectedPlantStore
      }
    }
    "write" - {
      "should be able to write decodable json" in {
        plantStoreFormat.read(
          plantStoreFormat.write(expectedPlantStore)
        ) shouldBe expectedPlantStore
      }
    }
  }

  "playerBoardFormat" - {
    "read" - {
      "should be able to read the following input" in {

      }
    }
  }

  "TokenFormat" - {
    "read" - {
      "should be able to read the following input" in {
        val examples =
          Table(
            ("jsonString", "expected"),
            ("tier1 19", ScoringTokenTierOne(19)),
            ("tier2 20", ScoringTokenTierTwo(20)),
            ("Tier3 30", ScoringTokenTierThree(30)),
            ("Tier4 40", ScoringTokenTierFour(40))
          )
        forAll(examples) { (jsonString, expected) =>
          ScoringTokenFormat.read(JsString(jsonString)) shouldBe expected
        }
      }
      "should be failed with the following inputs" in {
        val examples =
          Table(
            "jsonString",
            "asdf asdf",
            "tier1 abcd",
            "tier1 abcd efgh"
          )

        forAll(examples) { jsonString =>
          verifyReadError[ScoringToken](JsString(jsonString), "invalid token value: expected example 'tier1 20', 'tier2 20', 'tier4 50'")
        }
        verifyReadError[ScoringToken](JsNumber(12345), "invalid token value: expected example 'tier1 20', 'tier2 20', 'tier4 50'")
      }
    }
    "write" - {
      "should be able to write decodable json" in {
        val examples =
          Table(
            "example",
            ScoringTokenTierOne(1),
            ScoringTokenTierTwo(2),
            ScoringTokenTierThree(3),
            ScoringTokenTierFour(4)
          )
        forAll(examples) { example =>
          ScoringTokenFormat.read(
            ScoringTokenFormat.write(example)
          ) shouldBe example
        }
      }
    }
  }

  "SunLocationFormat" - {
    "read" - {
      "should be able to read the following input" in {
        val examples =
          Table(
            ("jsonInput", "expected"),
            (0, SunLocation0),
            (1, SunLocation1),
            (2, SunLocation2),
            (3, SunLocation3),
            (4, SunLocation4),
            (5, SunLocation5)
          )
        forAll(examples) { (example, expected) =>
          SunLocationFormat.read(JsNumber(example)) shouldBe expected
        }
      }
      "should be failed with the following input" in {
        val examples =
          Table(
            "jsonInput",
            JsNumber(9),
            JsString("abc"),
          )
        forAll(examples) { (example) =>
          verifyReadError[SunLocation](example, "invalid sun location value: expected 0-5")
        }
      }
    }
    "write" - {
      "should be able to write decodable json" in {
        val examples =
          Table(
            "example",
            SunLocation0,
            SunLocation1,
            SunLocation2,
            SunLocation3,
            SunLocation4,
            SunLocation5
          )
        forAll(examples) { example =>
          SunLocationFormat.read(
            SunLocationFormat.write(example)
          ) shouldBe example
        }
      }
    }
  }
  private def verifyReadError[A](sourceJson: JsValue, expectedError: String)(implicit jsonFormat: JsonFormat[A]): Any = {
    Try(jsonFormat.read(sourceJson)) match {
      case Failure(exception) => exception.getMessage shouldBe expectedError
      case _ => assert(false)
    }
  }

  "GameEngineFormats" - {

    "gameEngineRegistrationStateFormat" - {

      val expectedRegistration = Registration(
        Player("John", Green) :: Player("Marry", Yellow) :: Player("Madonna", Blue) :: Nil,
        ScoringTokenStacks(
          List(12, 13, 11).map(ScoringTokenTierOne),
          List(22, 21, 23).map(ScoringTokenTierTwo),
          List(25, 27, 28).map(ScoringTokenTierThree),
          List(30, 31, 32).map(ScoringTokenTierFour)
        )
      )

      "read" - {

        "should be able to decode the following input" in {
          val jsonObj =
            """
              |{
              |  "state": "registration",
              |  "players": [
              |    { "name": "John", "plantType": "green"},
              |    { "name": "Marry", "plantType": "yellow"},
              |    { "name": "Madonna", "plantType": "blue"}
              |  ],
              |  "scoringTokenStacks": {
              |    "tier1": ["tier1 12", "tier1 13", "tier1 11"],
              |    "tier2": ["tier2 22", "tier2 21", "tier2 23"],
              |    "tier3": ["tier3 25", "tier3 27", "tier3 28"],
              |    "tier4": ["tier4 30", "tier4 31", "tier4 32"]
              |  }
              |}
              |""".stripMargin.parseJson

          registrationFormat.read(jsonObj) shouldBe expectedRegistration
          GameEngineFormats.read(jsonObj) shouldBe expectedRegistration
        }

        "should fail if token is in the wrong tier" in {
          val jsonObj =
            """
              |{
              |  "state": "registration",
              |  "players": [
              |    { "name": "John", "plantType": "green"},
              |    { "name": "Marry", "plantType": "yellow"},
              |    { "name": "Madonna", "plantType": "blue"}
              |  ],
              |  "scoringTokenStacks": {
              |    "tier1": ["tier1 12", "tier1 13", "tier1 11"],
              |    "tier2": ["tier2 22", "tier2 21", "tier2 23"],
              |    "tier3": ["tier3 25", "tier3 27", "tier3 28"],
              |    "tier4": ["tier4 30", "tier4 31", "tier3 32"]
              |  }
              |}
              |""".stripMargin.parseJson

          verifyReadError[Registration](jsonObj, "expected scoring token tier4")
        }

      }

      "write" - {
        "should be able to write and read" in {
          registrationFormat.read(
            registrationFormat.write(expectedRegistration)
          ) shouldBe expectedRegistration

          GameEngineFormats.read(
            GameEngineFormats.write(expectedRegistration)
          ) shouldBe expectedRegistration
        }
      }
    }

    "gameEnginePlacingFirst2TreesStateFormat" - {

      val expectedSettingUp = SettingUp(
        activePlayerPosition = 0,
        playerBoards = List(Player("John", Green), Player("Marry", Yellow), Player("Madonna", Blue)).map(_.initBoard),
        blocks = Nil,
        scoringTokenStacks = ScoringTokenStacks(
          List(12, 13, 11).map(ScoringTokenTierOne),
          List(22, 21, 23).map(ScoringTokenTierTwo),
          List(25, 27, 28).map(ScoringTokenTierThree),
          List(30, 31, 32).map(ScoringTokenTierFour)
        )
      )

      "read" in {

        import spray.json.DefaultJsonProtocol._
        import spray.json._

        val jsonObj =
          s"""
             |{
             |  "state": "settingup",
             |  "activePlayerPosition": 0,
             |  "playerBoards": ${expectedSettingUp.playerBoards.map(playerBoard => playerBoardFormat.write(playerBoard)).toJson},
             |  "blocks": ${expectedSettingUp.blocks.map(forestBlock => blockFormat.write(forestBlock)).toJson},
             |  "scoringTokenStacks": {
             |    "tier1": ["tier1 12", "tier1 13", "tier1 11"],
             |    "tier2": ["tier2 22", "tier2 21", "tier2 23"],
             |    "tier3": ["tier3 25", "tier3 27", "tier3 28"],
             |    "tier4": ["tier4 30", "tier4 31", "tier4 32"]
             |  }
             |}
             |""".stripMargin.parseJson

        settingUpFormat.read(jsonObj) shouldBe expectedSettingUp
        GameEngineFormats.read(jsonObj) shouldBe expectedSettingUp
      }

      "write" in {
        settingUpFormat.read(
          settingUpFormat.write(expectedSettingUp)
        ) shouldBe expectedSettingUp

        GameEngineFormats.read(
          GameEngineFormats.write(expectedSettingUp)
        ) shouldBe expectedSettingUp
      }
    }

    "gameEnginePlayingFormat" - {

      val expectedPlaying = Playing(
        activePlayerPosition = 0,
        firstPlayerTokenPosition = 0,
        sunLocation = SunLocation0,
        day = 0,
        playerBoards = List(Player("John", Green), Player("Marry", Yellow), Player("Madonna", Blue)).map(_.initBoard),
        blocks = Nil,
        scoringTokenStacks = ScoringTokenStacks(
          List(12, 13, 11).map(ScoringTokenTierOne),
          List(22, 21, 23).map(ScoringTokenTierTwo),
          List(25, 27, 28).map(ScoringTokenTierThree),
          List(30, 31, 32).map(ScoringTokenTierFour)
        )
      )

      "read" in {

        import spray.json.DefaultJsonProtocol._
        import spray.json._

        val jsonObj =
          s"""
             |{
             |  "state": "playing",
             |  "activePlayerPosition": 0,
             |  "firstPlayerTokenPosition": 0,
             |  "sunLocation": 0,
             |  "day": 0,
             |  "playerBoards": ${expectedPlaying.playerBoards.map(playerBoard => playerBoardFormat.write(playerBoard)).toJson},
             |  "blocks": ${expectedPlaying.blocks.map(forestBlock => blockFormat.write(forestBlock)).toJson},
             |  "scoringTokenStacks": {
             |    "tier1": ["tier1 12", "tier1 13", "tier1 11"],
             |    "tier2": ["tier2 22", "tier2 21", "tier2 23"],
             |    "tier3": ["tier3 25", "tier3 27", "tier3 28"],
             |    "tier4": ["tier4 30", "tier4 31", "tier4 32"]
             |  }
             |}
             |""".stripMargin.parseJson

        playingFormat.read(jsonObj) shouldBe expectedPlaying
        GameEngineFormats.read(jsonObj) shouldBe expectedPlaying
      }

      "write" in {
        playingFormat.read(
          playingFormat.write(expectedPlaying)
        ) shouldBe expectedPlaying

        GameEngineFormats.read(
          GameEngineFormats.write(expectedPlaying)
        ) shouldBe expectedPlaying
      }
    }

    "gameEngineOverFormat" - {

      val expectedGameOver = GameOver(
        playerBoards = List(Player("John", Green), Player("Marry", Yellow), Player("Madonna", Blue)).map(_.initBoard),
        blocks = Nil
      )

      "read" in {

        import spray.json.DefaultJsonProtocol._
        import spray.json._

        val jsonObj =
          s"""
             |{
             |  "state": "over",
             |  "playerBoards": ${expectedGameOver.playerBoards.map(playerBoard => playerBoardFormat.write(playerBoard)).toJson},
             |  "blocks": ${expectedGameOver.blocks.map(forestBlock => blockFormat.write(forestBlock)).toJson}
             |}
             |""".stripMargin.parseJson

        gameOverFormat.read(jsonObj) shouldBe expectedGameOver
        GameEngineFormats.read(jsonObj) shouldBe expectedGameOver
      }

      "write" in {
        gameOverFormat.read(
          gameOverFormat.write(expectedGameOver)
        ) shouldBe expectedGameOver

        GameEngineFormats.read(
          GameEngineFormats.write(expectedGameOver)
        ) shouldBe expectedGameOver
      }
    }
  }
}

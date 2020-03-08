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

  }

  private def verifyReadError[A](sourceJson: JsValue, expectedError: String)(implicit jsonFormat: JsonFormat[A]): Any = {
    Try(jsonFormat.read(sourceJson)) match {
      case Failure(exception) => exception.getMessage shouldBe expectedError
      case _ => assert(false)
    }
  }
}

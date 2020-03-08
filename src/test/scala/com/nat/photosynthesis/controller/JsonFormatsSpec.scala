package com.nat.photosynthesis.controller

import org.scalatest.prop.TableDrivenPropertyChecks._
import com.nat.photosynthesis.model._
import org.scalatest.{FreeSpec, Matchers}

import scala.util.{Failure, Try}

class JsonFormatsSpec extends FreeSpec with Matchers {

  import JsonFormats._
  import spray.json._

  "PlantTypeFormat" - {
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
      Try(PlantTypeFormat.read(JsNumber(1))) match {
        case Failure(ex) => ex.getMessage shouldBe "invalid plant type value: expected string"
        case _ => assert(false)
      }
    }
  }

  "boardLocationFormat" - {
    "should return object when decode success" in {
      val sourceString = """{ "x": 1, "y": 2, "z": 3 }""".parseJson
      boardLocationFormat.read(sourceString) shouldBe BoardLocation(1, 2, 3)
    }
    "should throw error when decode fail" in {
      val sourceString = """{ "y": 2, "z": 3 }""".parseJson
      Try(boardLocationFormat.read(sourceString)) match {
        case Failure(exception) => exception.getMessage shouldBe "Object is missing required member 'x'"
        case _ => assert(false)
      }
    }
  }

  "playerFormat" - {
    "should decode the following json correctly" in {
      val sourceJson = """{ "name": "John", "plantType": "green"}""".parseJson
      playerFormat.read(sourceJson) shouldBe Player("John", Green)
    }

    "should decode fail if input incorrect player field" in {
      val sourceJson = """{ "namee": "John", "plantType": "green"}""".parseJson
      Try(playerFormat.read(sourceJson))  match {
        case Failure(exception) => exception.getMessage shouldBe "Object is missing required member 'name'"
        case _ => assert(false)
      }
    }

    "should fail if unable to decode plantType" in {
      val sourceJson = """{ "name": "John", "plantType": "greeeen"}""".parseJson
      Try(playerFormat.read(sourceJson)) match {
        case Failure(exception) => exception.getMessage shouldBe "invalid plant type value: expected (green, yellow, blue, orange)"
        case _ => assert(false)
      }
    }

  }
}

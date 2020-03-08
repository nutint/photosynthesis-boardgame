package com.nat.photosynthesis.controller

import com.nat.photosynthesis.model._

object JsonFormats {

  import spray.json.DefaultJsonProtocol._
  import spray.json._

  implicit object GameEngineFormats extends JsonFormat[GameEngine] {
    override def write(obj: GameEngine): JsValue = ???

    override def read(json: JsValue): GameEngine = ???
  }

  implicit object PlantTypeFormat extends JsonFormat[PlantType] {
    override def write(plantType: PlantType): JsValue = JsString(plantType match {
      case Green => "green"
      case Yellow => "yellow"
      case Orange => "orange"
      case Blue => "blue"
    })

    override def read(json: JsValue): PlantType = json match {
      case JsString(str) => str.trim.toLowerCase match {
        case "green" => Green
        case "yellow" => Yellow
        case "orange" => Orange
        case "blue" => Blue
        case _ => deserializationError("invalid plant type value: expected (green, yellow, blue, orange)")
      }
      case _ => deserializationError("invalid plant type value: expected string")
    }
  }

  implicit object BoardLocationTierFormat extends JsonFormat[BoardLocationTier] {
    override def read(json: JsValue): BoardLocationTier = json match {
      case JsString(strVal) => strVal.trim.toLowerCase match {
        case "tier1" => BoardLocationTier1
        case "tier2" => BoardLocationTier2
        case "tier3" => BoardLocationTier3
        case "tier4" => BoardLocationTier4
        case _ => deserializationError("invalid board location tier value: expected (tier1, tier2, tier3, tier4)")
      }
      case _ => deserializationError("invalid board location tier value: expected string")
    }

    override def write(obj: BoardLocationTier): JsValue = JsString(obj match {
      case BoardLocationTier1 => "tier1"
      case BoardLocationTier2 => "tier2"
      case BoardLocationTier3 => "tier3"
      case BoardLocationTier4 => "tier4"
    })
  }

  implicit val boardLocationFormat = jsonFormat3(BoardLocation.apply)
  implicit val playerFormat = jsonFormat2(Player.apply)
}

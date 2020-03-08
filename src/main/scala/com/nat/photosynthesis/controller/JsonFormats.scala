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

  implicit val boardLocationFormat = jsonFormat3(BoardLocation.apply)
  implicit val playerFormat = jsonFormat2(Player.apply)
}

package com.nat.photosynthesis.controller

import com.nat.photosynthesis.model._

import scala.util._

object JsonFormats {

  import spray.json.DefaultJsonProtocol._
  import spray.json._

  implicit object GameEngineFormats extends JsonFormat[GameEngine] {
    override def write(obj: GameEngine): JsValue = ???

    override def read(json: JsValue): GameEngine = ???
  }

  implicit object PlantTypeFormat extends JsonFormat[PlantType] {
    override def write(plantType: PlantType): JsValue = JsString(plantTypeToString(plantType))

    def plantTypeToString(plantType: PlantType) = {
      plantType match {
        case Green => "green"
        case Yellow => "yellow"
        case Orange => "orange"
        case Blue => "blue"
      }
    }

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

  implicit object DistanceFormat extends JsonFormat[Distance] {
    val errorMsg = "invalid distance value: expected (same, differentline, front x, rear x)"
    override def read(json: JsValue): Distance = json match {
      case JsString(strVal) =>
        strVal.trim.toLowerCase.split(" ").map(_.trim).toList match {
        case st :: Nil => st match {
          case "same" => Same
          case "differentline" => DifferentLine
          case _ => deserializationError(errorMsg)
        }
        case st :: nd :: Nil => (st, Try(nd.toInt)) match {
          case ("front", Success(intVal)) => Front(intVal)
          case ("rear", Success(intVal)) => Rear(intVal)
          case (_, _) => deserializationError(errorMsg)
        }
        case _ => deserializationError(errorMsg)
      }
      case _ => deserializationError(errorMsg)
    }

    override def write(obj: Distance): JsValue = JsString( obj match {
      case f: Front => s"front ${f.d}"
      case r: Rear => s"rear ${r.d}"
      case Same => "same"
      case DifferentLine => "differentline"
    })
  }

  implicit object PlantItemFormat extends JsonFormat[PlantItem] {
    val errorMsg = "invalid plant item value: expected example 'green seed', 'yellow medium-tree', 'blue cooleddown-medium-tree"
    override def read(json: JsValue): PlantItem = json match {
      case JsString(strVal) => strVal.trim.toLowerCase.split(" ").map(_.trim).toList match {
        case fst :: snd :: Nil => {
          Try(PlantTypeFormat.read(JsString(fst))) match {
            case Success(plantType) =>
              snd match {
                case "seed" => Seed(plantType)
                case "cooleddown-small-tree" => CooledDownSmallTree(plantType)
                case "small-tree" => SmallTree(plantType)
                case "cooleddown-medium-tree" => CooledDownMediumTree(plantType)
                case "medium-tree" => MediumTree(plantType)
                case "cooleddown-large-tree" => CooledDownLargeTree(plantType)
                case "large-tree" => LargeTree(plantType)
                case _ => deserializationError(errorMsg)
              }
            case Failure(_) => deserializationError(errorMsg)
          }
        }
        case _ => deserializationError(errorMsg)
      }
      case _ => deserializationError(errorMsg)
    }

    override def write(obj: PlantItem): JsValue = {
      val plantTypeString = PlantTypeFormat.plantTypeToString(obj.plantType)
      JsString(obj match {
        case _: Seed => s"$plantTypeString seed"
        case _: CooledDownSmallTree => s"$plantTypeString cooleddown-small-tree"
        case _: SmallTree => s"$plantTypeString small-tree"
        case _: CooledDownMediumTree => s"$plantTypeString cooleddown-medium-tree"
        case _: MediumTree => s"$plantTypeString medium-tree"
        case _: CooledDownLargeTree => s"$plantTypeString cooleddown-large-tree"
        case _: LargeTree => s"$plantTypeString large-tree"
      })
    }
  }

  implicit val boardLocationFormat = jsonFormat3(BoardLocation.apply)
  implicit val playerFormat = jsonFormat2(Player.apply)
  implicit def storeSpaceFormat[A<:PlantItem] = jsonFormat2(StoreSpace.apply[A])
}

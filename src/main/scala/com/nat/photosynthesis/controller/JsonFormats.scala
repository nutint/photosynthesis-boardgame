package com.nat.photosynthesis.controller

import com.nat.photosynthesis.service.model._
import com.nat.photosynthesis.service.model.engine.{GameEngine, GameOver, Playing, Registration, SettingUp}

import scala.util._

object JsonFormats {

  import spray.json.DefaultJsonProtocol._
  import spray.json._

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

  implicit object BoardLocationTierFormat extends JsonFormat[LocationTier] {
    override def read(json: JsValue): LocationTier = json match {
      case JsString(strVal) => strVal.trim.toLowerCase match {
        case "tier1" => LocationTier1$
        case "tier2" => LocationTier2$
        case "tier3" => LocationTier3$
        case "tier4" => LocationTier4$
        case _ => deserializationError("invalid board location tier value: expected (tier1, tier2, tier3, tier4)")
      }
      case _ => deserializationError("invalid board location tier value: expected string")
    }

    override def write(obj: LocationTier): JsValue = JsString(obj match {
      case LocationTier1$ => "tier1"
      case LocationTier2$ => "tier2"
      case LocationTier3$ => "tier3"
      case LocationTier4$ => "tier4"
    })
  }

  implicit object DistanceFormat extends JsonFormat[Displacement] {
    val errorMsg = "invalid distance value: expected (same, differentline, front x, rear x)"
    override def read(json: JsValue): Displacement = json match {
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

    override def write(obj: Displacement): JsValue = JsString( obj match {
      case f: Front => s"front ${f.d}"
      case r: Rear => s"rear ${r.d}"
      case Same => "same"
      case DifferentLine => "differentline"
    })
  }

  implicit object PlantItemFormat extends JsonFormat[Plant] {
    val errorMsg = "invalid plant item value: expected example 'green seed', 'yellow medium-tree', 'blue cooleddown-medium-tree"
    override def read(json: JsValue): Plant = json match {
      case JsString(strVal) => strVal.trim.toLowerCase.split(" ").map(_.trim).toList match {
        case fst :: snd :: Nil => {
          Try(PlantTypeFormat.read(JsString(fst))) match {
            case Success(plantType) =>
              snd match {
                case "seed" => Seed(plantType)
                case "cooleddown-small-tree" => CoolingDownSmallTree(plantType)
                case "small-tree" => SmallTree(plantType)
                case "cooleddown-medium-tree" => CoolingDownMediumTree(plantType)
                case "medium-tree" => MediumTree(plantType)
                case "cooleddown-large-tree" => CoolingDownLargeTree(plantType)
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

    override def write(obj: Plant): JsValue = {
      val plantTypeString = PlantTypeFormat.plantTypeToString(obj.plantType)
      JsString(obj match {
        case _: Seed => s"$plantTypeString seed"
        case _: CoolingDownSmallTree => s"$plantTypeString cooleddown-small-tree"
        case _: SmallTree => s"$plantTypeString small-tree"
        case _: CoolingDownMediumTree => s"$plantTypeString cooleddown-medium-tree"
        case _: MediumTree => s"$plantTypeString medium-tree"
        case _: CoolingDownLargeTree => s"$plantTypeString cooleddown-large-tree"
        case _: LargeTree => s"$plantTypeString large-tree"
      })
    }
  }

  implicit object TokenFormat extends JsonFormat[ScoringToken] {
    val errorMsg = "invalid token value: expected example 'tier1 20', 'tier2 20', 'tier4 50'"
    override def read(json: JsValue): ScoringToken = json match {
      case JsString(strVal) => strVal.trim.toLowerCase.split(" ").map(_.trim).toList match {
        case fst :: snd :: Nil =>
          (fst, Try(snd.toInt)) match {
            case ("tier1", Success(v)) => ScoringTokenTierOne(v)
            case ("tier2", Success(v)) => ScoringTokenTierTwo(v)
            case ("tier3", Success(v)) => ScoringTokenTierThree(v)
            case ("tier4", Success(v)) => ScoringTokenTierFour(v)
            case (_, Success(_)) => deserializationError(errorMsg)
            case _ => deserializationError(errorMsg)
          }
        case _ => deserializationError(errorMsg)
      }
      case _ => deserializationError(errorMsg)
    }
    override def write(obj: ScoringToken): JsValue = JsString(obj match {
      case ScoringTokenTierOne(v) => s"tier1 $v"
      case ScoringTokenTierTwo(v) => s"tier2 $v"
      case ScoringTokenTierThree(v) => s"tier3 $v"
      case ScoringTokenTierFour(v) => s"tier4 $v"
    })
  }

  implicit object SunLocationFormat extends JsonFormat[SunLocation] {
    val errorMsg = "invalid sun location value: expected 0-5"
    override def read(json: JsValue): SunLocation = json match {
      case JsNumber(n) => n.toInt match {
        case 0 => SunLocation0
        case 1 => SunLocation1
        case 2 => SunLocation2
        case 3 => SunLocation3
        case 4 => SunLocation4
        case 5 => SunLocation5
        case _ => deserializationError(errorMsg)
      }
      case _ => deserializationError(errorMsg)
    }
    override def write(obj: SunLocation): JsValue = JsNumber(obj match {
      case SunLocation0 => 0
      case SunLocation1 => 1
      case SunLocation2 => 2
      case SunLocation3 => 3
      case SunLocation4 => 4
      case SunLocation5 => 5
    })
  }

  implicit val tokenTierOneFormat: JsonFormat[ScoringTokenTierOne] = reuseFormat[ScoringTokenTierOne, ScoringToken](_.isInstanceOf[ScoringTokenTierOne])
  implicit val tokenTierTwoFormat: JsonFormat[ScoringTokenTierTwo] = reuseFormat[ScoringTokenTierTwo, ScoringToken](_.isInstanceOf[ScoringTokenTierTwo])
  implicit val tokenTierTreeFormat: JsonFormat[ScoringTokenTierThree] = reuseFormat[ScoringTokenTierThree, ScoringToken](_.isInstanceOf[ScoringTokenTierThree])
  implicit val tokenTierFourFormat: JsonFormat[ScoringTokenTierFour] = reuseFormat[ScoringTokenTierFour, ScoringToken](_.isInstanceOf[ScoringTokenTierFour])

  def reuseFormat[A<:B, B](isKindOf: B => Boolean)(implicit superFormat: JsonFormat[B]): JsonFormat[A] = new JsonFormat[A] {
    override def write(obj: A): JsValue = superFormat.write(obj)
    override def read(json: JsValue): A = superFormat.read(json) match {
      case t if isKindOf(t) => t.asInstanceOf[A]
      case _ => deserializationError("abc")
    }
  }

  implicit val boardLocationFormat = jsonFormat3(Location.apply)
  implicit val playerFormat = jsonFormat2(Player.apply)
  implicit def storeSpaceFormat[A<:Plant] = jsonFormat2(StoreSpace.apply[A])
  implicit val plantStoreFormat = jsonFormat5(PlantStore.apply)
  implicit val playerBoardFormat = jsonFormat5(PlayerBoard.apply)
  implicit val tokenStockFormat = jsonFormat4(TokenStock.apply)
  implicit val forestBlockFormat = jsonFormat2(Block.apply)

  implicit val gameEngineRegistrationStateFormat = jsonFormat2(Registration.apply)
  implicit val gameEnginePlacingFirst2TreesStateFormat = jsonFormat4(SettingUp.apply)
  implicit val gameEnginePlayingFormat = jsonFormat7(Playing.apply)
  implicit val gameEngineOverFormat = jsonFormat2(GameOver.apply)

  implicit object GameEngineFormats extends JsonFormat[GameEngine] {
    case class GameEngineStateDetector(state: String)
    implicit val gesdFormat = jsonFormat1(GameEngineStateDetector.apply)

    override def write(obj: GameEngine): JsValue = ???
//
//    override def write(obj: GameEngine): JsValue = {
//      val (json, key) = (obj match {
//        case ge: GameEngineRegistrationState => (gameEngineRegistrationStateFormat.write(ge), "registration")
//        case ge: GameEnginePlacingFirst2TreesState => (gameEnginePlacingFirst2TreesStateFormat.write(ge), "placing2trees")
//        case ge: GameEnginePlaying => (gameEnginePlayingFormat.write(ge), "playing")
//        case ge: GameEngineOver => (gameEngineOverFormat.write(ge), "over")
//      })
//      JsObject(json.asJsObject.fields + ("state" -> key))
//    }

    override def read(json: JsValue): GameEngine = gesdFormat.read(json) match {
      case GameEngineStateDetector(state) => state.trim.toLowerCase() match {
        case "registration" => gameEngineRegistrationStateFormat.read(json)
        case "placing2trees" => gameEnginePlacingFirst2TreesStateFormat.read(json)
        case "playing" => gameEnginePlayingFormat.read(json)
        case "over" => gameEngineOverFormat.read(json)
      }
    }
  }
}

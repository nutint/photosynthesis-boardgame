package com.nat.photosynthesis.controller

import com.nat.photosynthesis.model._
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

  implicit object TokenFormat extends JsonFormat[Token] {
    val errorMsg = "invalid token value: expected example 'tier1 20', 'tier2 20', 'tier4 50'"
    override def read(json: JsValue): Token = json match {
      case JsString(strVal) => strVal.trim.toLowerCase.split(" ").map(_.trim).toList match {
        case fst :: snd :: Nil =>
          (fst, Try(snd.toInt)) match {
            case ("tier1", Success(v)) => TokenTierOne(v)
            case ("tier2", Success(v)) => TokenTierTwo(v)
            case ("tier3", Success(v)) => TokenTierThree(v)
            case ("tier4", Success(v)) => TokenTierFour(v)
            case (_, Success(_)) => deserializationError(errorMsg)
            case _ => deserializationError(errorMsg)
          }
        case _ => deserializationError(errorMsg)
      }
      case _ => deserializationError(errorMsg)
    }
    override def write(obj: Token): JsValue = JsString(obj match {
      case TokenTierOne(v) => s"tier1 $v"
      case TokenTierTwo(v) => s"tier2 $v"
      case TokenTierThree(v) => s"tier3 $v"
      case TokenTierFour(v) => s"tier4 $v"
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

  implicit val tokenTierOneFormat: JsonFormat[TokenTierOne] = reuseFormat[TokenTierOne, Token](_.isInstanceOf[TokenTierOne])
  implicit val tokenTierTwoFormat: JsonFormat[TokenTierTwo] = reuseFormat[TokenTierTwo, Token](_.isInstanceOf[TokenTierTwo])
  implicit val tokenTierTreeFormat: JsonFormat[TokenTierThree] = reuseFormat[TokenTierThree, Token](_.isInstanceOf[TokenTierThree])
  implicit val tokenTierFourFormat: JsonFormat[TokenTierFour] = reuseFormat[TokenTierFour, Token](_.isInstanceOf[TokenTierFour])

  def reuseFormat[A<:B, B](isKindOf: B => Boolean)(implicit superFormat: JsonFormat[B]): JsonFormat[A] = new JsonFormat[A] {
    override def write(obj: A): JsValue = superFormat.write(obj)
    override def read(json: JsValue): A = superFormat.read(json) match {
      case t if isKindOf(t) => t.asInstanceOf[A]
      case _ => deserializationError("abc")
    }
  }

  implicit val boardLocationFormat = jsonFormat3(BoardLocation.apply)
  implicit val playerFormat = jsonFormat2(Player.apply)
  implicit def storeSpaceFormat[A<:PlantItem] = jsonFormat2(StoreSpace.apply[A])
  implicit val plantStoreFormat = jsonFormat5(PlantStore.apply)
  implicit val playerBoardFormat = jsonFormat5(PlayerBoard.apply)
  implicit val tokenStockFormat = jsonFormat4(TokenStock.apply)
  implicit val forestBlockFormat = jsonFormat2(ForestBlock.apply)

  implicit val gameEngineRegistrationStateFormat = jsonFormat2(GameEngineRegistrationState.apply)
  implicit val gameEnginePlacingFirst2TreesStateFormat = jsonFormat4(GameEnginePlacingFirst2TreesState.apply)
  implicit val gameEnginePlayingFormat = jsonFormat7(GameEnginePlaying.apply)
  implicit val gameEngineOverFormat = jsonFormat2(GameEngineOver.apply)

  implicit object GameEngineFormats extends JsonFormat[GameEngine] {
    case class GameEngineStateDetector(state: String)
    implicit val gesdFormat = jsonFormat1(GameEngineStateDetector.apply)

    override def write(obj: GameEngine): JsValue = {
      val (json, key) = (obj match {
        case ge: GameEngineRegistrationState => (gameEngineRegistrationStateFormat.write(ge), "registration")
        case ge: GameEnginePlacingFirst2TreesState => (gameEnginePlacingFirst2TreesStateFormat.write(ge), "placing2trees")
        case ge: GameEnginePlaying => (gameEnginePlayingFormat.write(ge), "playing")
        case ge: GameEngineOver => (gameEngineOverFormat.write(ge), "over")
      })
      JsObject(json.asJsObject.fields + ("state" -> key))
    }

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

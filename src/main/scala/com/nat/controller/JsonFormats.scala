package com.nat.controller

import akka.http.javadsl.model.StatusCode
import com.nat.model.Identifiable

object JsonFormats {

  import spray.json.DefaultJsonProtocol._
  import spray.json._

  case class DataResponse[A](data: Option[A], code: Int, status: String)

  implicit def dataResponseFormat[A:JsonFormat] = jsonFormat3(DataResponse.apply[A])
  implicit def identifiableFormat[A:JsonFormat] = jsonFormat2(Identifiable.apply[A])
  implicit def akkaCodesToInt(akkaCode: StatusCode) = akkaCode.intValue
  implicit def akkaCodesToString(akkaCode: StatusCode) = akkaCode.toString.substring(4)
}

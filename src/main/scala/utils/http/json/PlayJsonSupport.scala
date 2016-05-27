package utils.http.json

import akka.http.scaladsl.marshalling.{ Marshaller, ToEntityMarshaller }
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.unmarshalling.{ FromEntityUnmarshaller, Unmarshaller }
import play.api.libs.json.{ JsValue, Json, Reads, Writes }
import utils.http.protocol.ValidationError

import scala.language.implicitConversions

object PlayJsonSupport extends PlayJsonSupport

trait PlayJsonSupport extends de.heikoseeberger.akkahttpplayjson.PlayJsonSupport {

  implicit val defaultPrinter: JsValue ⇒ String = Json.stringify

}

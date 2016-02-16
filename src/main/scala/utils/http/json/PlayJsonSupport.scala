package utils.http.json

import akka.http.scaladsl.marshalling._
import akka.http.scaladsl.model.{ HttpEntity, MessageEntity, HttpCharsets, MediaTypes }
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.http.scaladsl.unmarshalling._
import play.api.libs.json._
import utils.http.protocol.ValidationError

import scala.concurrent.{ ExecutionContext, Future }
import scala.language.implicitConversions

object PlayJsonSupport extends PlayJsonSupport

trait PlayJsonSupport {

  val defaultPrinter: JsValue ⇒ String = Json.prettyPrint

  def onJsonMarshalling(json: JsValue)(implicit ec: ExecutionContext, ctx: JsonMarshallingContext): Future[JsValue] = Future.successful(json)

  def extractJsonMarshallingContext: Directive1[JsonMarshallingContext] = provide(null)

  implicit def playJsonUnmarshallerConverter[A](reads: Reads[A]): FromEntityUnmarshaller[A] =
    playJsonUnmarshaller(reads)

  implicit def playJsonUnmarshaller[A](implicit reads: Reads[A]): FromEntityUnmarshaller[A] = {
    def read(json: JsValue) = reads.reads(json).recoverTotal(error ⇒ throw ValidationError.playJson(error.errors))
    playJsValueUnmarshaller.map(read)
  }

  implicit def playJsValueUnmarshaller: FromEntityUnmarshaller[JsValue] =
    Unmarshaller
      .byteStringUnmarshaller
      .forContentTypes(MediaTypes.`application/json`)
      .mapWithCharset { (data, charset) ⇒
        val input = if (charset == HttpCharsets.`UTF-8`) data.utf8String else data.decodeString(charset.nioCharset.name)
        Json.parse(input)
      }

  implicit def playJsonMarshallerConverter[A](writes: Writes[A])(implicit printer: JsValue ⇒ String = defaultPrinter, ctx: JsonMarshallingContext = null): ToEntityMarshaller[A] =
    playJsonMarshaller[A](writes, printer, ctx)

  implicit def playJsonMarshaller[A](implicit writes: Writes[A], printer: JsValue ⇒ String = defaultPrinter, ctx: JsonMarshallingContext = null): ToEntityMarshaller[A] =
    playJsValueMarshaller.compose(writes.writes)

  implicit def playJsValueMarshaller(implicit printer: JsValue ⇒ String = defaultPrinter, ctx: JsonMarshallingContext = null): ToEntityMarshaller[JsValue] =
    Marshaller[JsValue, MessageEntity]{ implicit ec ⇒ js ⇒
      Option(ctx)
        .fold(Future.successful(js))(c ⇒ onJsonMarshalling(js)(ec, c))
        .map(json ⇒ Marshalling.WithFixedContentType[MessageEntity](MediaTypes.`application/json`, () ⇒ HttpEntity(MediaTypes.`application/json`, printer(json))) :: Nil)
    }
}

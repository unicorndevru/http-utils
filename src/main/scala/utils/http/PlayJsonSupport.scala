package utils.http

import akka.http.scaladsl.marshalling._
import akka.http.scaladsl.model.{ HttpCharsets, MediaTypes }
import akka.http.scaladsl.unmarshalling._
import play.api.libs.json._
import utils.http.protocol.ValidationError

import scala.language.implicitConversions

object PlayJsonSupport extends PlayJsonSupport

trait PlayJsonSupport {

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

  implicit def playJsonMarshallerConverter[A](writes: Writes[A])(implicit printer: JsValue ⇒ String = Json.prettyPrint): ToEntityMarshaller[A] =
    playJsonMarshaller[A](writes, printer)

  implicit def playJsonMarshaller[A](implicit writes: Writes[A], printer: JsValue ⇒ String = Json.prettyPrint): ToEntityMarshaller[A] =
    playJsValueMarshaller.compose(writes.writes)

  implicit def playJsValueMarshaller(implicit printer: JsValue ⇒ String = Json.prettyPrint): ToEntityMarshaller[JsValue] =
    Marshaller.StringMarshaller.wrap(MediaTypes.`application/json`)(printer)
}

package utils.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{ Directives, ExceptionHandler }
import play.api.libs.json.{ Json, Writes }
import utils.http.protocol.{ ApiError, ValidationError }

object ApiErrorHandler extends PlayJsonSupport {
  implicit val authErrorEncoder: Writes[ApiError] = Writes(err ⇒
    Json.obj("code" → err.code, "desc" → err.desc))

  implicit val validationErrorEncoder: Writes[ValidationError] = Writes(err ⇒
    Json.obj("code" → err.code, "desc" → err.desc, "fields" → err.fields))

  val generic = ExceptionHandler {
    case e: ValidationError ⇒
      Directives.complete(StatusCodes.custom(e.statusCode, e.desc) → Json.toJson(e)(validationErrorEncoder))

    case e: ApiError ⇒
      Directives.complete(StatusCodes.custom(e.statusCode, e.desc) → e)
  }
}

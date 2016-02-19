package utils.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{ MalformedRequestContentRejection, RejectionHandler, Directives, ExceptionHandler }
import play.api.libs.json.{ Json, Writes }
import utils.http.json.PlayJsonSupport
import utils.http.protocol.{ ApiError, ValidationError }

object ApiErrorHandler extends PlayJsonSupport {
  implicit val authErrorEncoder: Writes[ApiError] = Writes(err ⇒
    Json.obj("code" → err.code, "desc" → err.desc))

  implicit val validationErrorEncoder: Writes[ValidationError] = Writes(err ⇒
    Json.obj("code" → err.code, "desc" → err.desc, "fields" → err.fields))

  implicit val exceptionHandler: ExceptionHandler = ExceptionHandler {
    case e: ValidationError ⇒
      Directives.complete(StatusCodes.custom(e.statusCode, e.desc) → Json.toJson(e)(validationErrorEncoder))

    case e: ApiError ⇒
      Directives.complete(StatusCodes.custom(e.statusCode, e.desc) → e)
  }

  implicit val rejectionHandler: RejectionHandler = RejectionHandler.newBuilder().handle {
    case MalformedRequestContentRejection(msg, Some(e: ValidationError)) ⇒
      Directives.complete(StatusCodes.custom(e.statusCode, e.desc) → Json.toJson(e)(validationErrorEncoder))
  }.result().withFallback(RejectionHandler.default)
}

package utils.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server._
import play.api.libs.json.{ JsResultException, Json, Writes }
import utils.http.json.PlayJsonSupport
import utils.http.protocol.ApiError.AuthorizationFailedError
import utils.http.protocol.{ ApiError, ValidationError }
import Directives.complete

object ApiErrorHandler extends PlayJsonSupport {
  implicit val authErrorEncoder: Writes[ApiError] = Writes(err ⇒
    Json.obj("code" → err.code, "desc" → err.desc))

  implicit val validationErrorEncoder: Writes[ValidationError] = Writes(err ⇒
    Json.obj("code" → err.code, "desc" → err.desc, "fields" → err.fields))

  implicit val exceptionHandler: ExceptionHandler = ExceptionHandler {
    case e: ValidationError ⇒
      complete(StatusCodes.custom(e.statusCode, e.desc) → Json.toJson(e)(validationErrorEncoder))

    case e: ApiError ⇒
      complete(StatusCodes.custom(e.statusCode, e.desc) → e)
  }

  implicit val rejectionHandler: RejectionHandler = RejectionHandler.newBuilder().handle {
    case MalformedRequestContentRejection(msg, e: JsResultException) ⇒
      val err = ValidationError.playJson(e.errors)
      complete(StatusCodes.custom(err.statusCode, err.desc) → Json.toJson(err)(validationErrorEncoder))

    case AuthorizationFailedRejection ⇒
      val e = AuthorizationFailedError
      complete(StatusCodes.custom(e.statusCode, e.desc) → Json.toJson(e))

  }.result().withFallback(RejectionHandler.default)
}

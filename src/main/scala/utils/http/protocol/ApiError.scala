package utils.http.protocol

import play.api.data.validation.{ ValidationError ⇒ JsonError }
import play.api.libs.json.JsPath

import scala.collection.Seq
import scala.util.control.NoStackTrace

class ApiError(val code: String, val desc: String, val statusCode: Int = 500) extends RuntimeException(desc) with NoStackTrace {
  def forField(name: String) = new ValidationError(fields = Map(name → this), statusCode = if (statusCode == 500) 400 else statusCode)
}

object ApiError {
  case object MalformedDataError extends ApiError("error.malformedData", "Malformed Data", 400)
}

class ValidationError(code: String = "validation.error", desc: String = "Validation Error", val fields: Map[String, ApiError] = Map.empty, statusCode: Int = 400) extends ApiError(code, desc, statusCode) {
  def combine(o: ValidationError): ValidationError = new ValidationError(fields = this.fields ++ o.fields, statusCode = Math.max(this.statusCode, o.statusCode))
}

object ValidationError extends ValidationError("validation.error", "Validation Error", Map.empty, 400) {
  def playJson(errors: Seq[(JsPath, Seq[JsonError])]): ValidationError =
    errors.map {
      case (path, error) ⇒
        new ApiError("validation.json", error.toString(), 400).forField(path.toString())
    }.reduceLeftOption(_ combine _).getOrElse(ValidationError)
}
package utils.http.json

import akka.http.scaladsl.model.HttpHeader

trait JsonMarshallingContext {
  def format: String = ""
  def headers: Seq[HttpHeader] = Seq.empty
}
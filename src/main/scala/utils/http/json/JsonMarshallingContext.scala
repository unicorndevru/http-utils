package utils.http.json

trait JsonMarshallingContext {
  def params: Map[String, String]
}

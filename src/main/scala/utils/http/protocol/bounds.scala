package utils.http.protocol

import akka.http.scaladsl.unmarshalling.{ FromStringUnmarshaller, Unmarshaller }

import scala.concurrent.Future

case class FilterBound[T](edge: T, strict: Boolean = false) {
  def toOp(prefix: String) = prefix + (if (strict) "" else "e")

  def map[K](f: T ⇒ K): FilterBound[K] = copy[K](edge = f(edge))
}

case class FilterBounds[T](left: Option[FilterBound[T]], right: Option[FilterBound[T]]) {
  def map[K](f: T ⇒ K): FilterBounds[K] = copy[K](left = left.map(_.map(f)), right = right.map(_.map(f)))
}

object FilterBounds {

  val leftQueryOp = "$gt"
  val rightQueryOp = "$lt"

  implicit def unmarshaller[T: FromStringUnmarshaller]: FromStringUnmarshaller[FilterBounds[T]] = Unmarshaller.withMaterializer[String, FilterBounds[T]] { implicit ctx ⇒ implicit mat ⇒ str ⇒

    val leftStrict =
      if (str.headOption.contains('(')) 0
      else if (str.headOption.contains('[')) 1
      else -1

    val rightStrict =
      if (str.lastOption.contains(')')) 0
      else if (str.lastOption.contains(']')) 1
      else -1

    if (rightStrict < 0 || leftStrict < 0) {
      Future.failed(ApiError.MalformedDataError)
    } else {
      val input = str.substring(1, str.length - 1)
      val i = input.indexOf(';')
      if (i < 0) {
        Future.failed(ApiError.MalformedDataError)
      } else {
        val (sleft, sright) = input.splitAt(i)
        val left = sleft.trim()
        val right = sright.drop(1).trim
        val u = implicitly[Unmarshaller[String, T]]
        (for {
          lo ← if (left.nonEmpty) u(left).map(Some(_)) else Future.successful(None)
          ro ← if (right.nonEmpty) u(right).map(Some(_)) else Future.successful(None)
        } yield FilterBounds[T](
          left = lo.map(l ⇒ FilterBound[T](edge = l, strict = leftStrict > 0)),
          right = ro.map(r ⇒ FilterBound[T](edge = r, strict = rightStrict > 0))
        )).flatMap {
          case FilterBounds(None, None) => Future.failed(ApiError.MalformedDataError)
          case result => Future.successful(result)
        }
      }
    }
  }
}

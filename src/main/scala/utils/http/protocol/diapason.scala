package utils.http.protocol

import akka.http.scaladsl.unmarshalling.{ FromStringUnmarshaller, Unmarshaller }

import scala.concurrent.Future

case class DiapasonSide[T](value: T, inclusive: Boolean = false) {
  def toOp(prefix: String) = prefix + (if (inclusive) "e" else "")

  def map[K](f: T ⇒ K): DiapasonSide[K] = copy[K](value = f(value))
}

case class Diapason[T](from: Option[DiapasonSide[T]], to: Option[DiapasonSide[T]]) {
  def map[K](f: T ⇒ K): Diapason[K] = copy[K](from = from.map(_.map(f)), to = to.map(_.map(f)))
}

object Diapason {

  implicit def unmarshaller[T: FromStringUnmarshaller]: FromStringUnmarshaller[Diapason[T]] =
    Unmarshaller.withMaterializer[String, Diapason[T]] { implicit ctx ⇒ implicit mat ⇒ str ⇒

      val fromInclusive =
        if (str.headOption.contains('(')) 0
        else if (str.headOption.contains('[')) 1
        else -1

      val toInclusive =
        if (str.lastOption.contains(')')) 0
        else if (str.lastOption.contains(']')) 1
        else -1

      if (toInclusive < 0 || fromInclusive < 0) {
        Future.failed(ApiError.MalformedDataError)
      } else {
        val input = str.substring(1, str.length - 1)
        val i = input.indexOf(';')
        if (i < 0) {
          Future.failed(ApiError.MalformedDataError)
        } else {
          val (sfrom, sto) = input.splitAt(i)
          val from = sfrom.trim()
          val to = sto.drop(1).trim
          val u = implicitly[Unmarshaller[String, T]]
          (for {
            fo ← if (from.nonEmpty) u(from).map(Some(_)) else Future.successful(None)
            too ← if (to.nonEmpty) u(to).map(Some(_)) else Future.successful(None)
          } yield Diapason[T](
            from = fo.map(f ⇒ DiapasonSide[T](value = f, inclusive = fromInclusive > 0)),
            to = too.map(t ⇒ DiapasonSide[T](value = t, inclusive = toInclusive > 0))
          )).flatMap {
            case Diapason(None, None) ⇒ Future.failed(ApiError.MalformedDataError)
            case result               ⇒ Future.successful(result)
          }
        }
      }
    }
}

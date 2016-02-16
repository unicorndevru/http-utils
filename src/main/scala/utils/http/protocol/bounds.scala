package utils.http.protocol

import akka.http.scaladsl.unmarshalling.{Unmarshal, Unmarshaller}
import akka.stream.Materializer

import scala.concurrent.{ExecutionContext, Future}

case class FilterBound[T](edge: T, strict: Boolean = false) {
  def toOp(prefix: String) = prefix + (if (strict) "" else "e")

  def map[K](f: T ⇒ K): FilterBound[K] = copy[K](edge = f(edge))
}

case class FilterBounds[T](left: Option[FilterBound[T]], right: Option[FilterBound[T]]) {

  import FilterBounds.{ leftQueryOp, rightQueryOp }

  def toParams(field: String): Seq[(String, String)] = {
    val empty = Seq.empty[(String, String)]
    left.fold(empty)(v ⇒ empty :+ ((field + v.toOp(leftQueryOp)) → v.edge.toString)) ++
      right.fold(empty)(v ⇒ empty :+ ((field + v.toOp(rightQueryOp)) → v.edge.toString))
  }

  def map[K](f: T ⇒ K): FilterBounds[K] = copy[K](left = left.map(_.map(f)), right = right.map(_.map(f)))
}

object FilterBounds {
  import scala.concurrent.ExecutionContext.Implicits.global

  val leftSigns = "(["
  val rightSigns = ")]"
  val separator = ";"

  val leftJsonOp = "$gt"
  val rightJsonOp = "$lt"

  val leftQueryOp = "$gt"
  val rightQueryOp = "$lt"

  def parseQueryParams[T](field: String, params: Map[String, Seq[String]], rv: String ⇒ Option[T]): Option[FilterBounds[T]] = {
    def readOne(op: String): Option[FilterBound[T]] =
      params.get(field + op).map(v ⇒ (v, true)).orElse(params.get(field + op + "e").map(v ⇒ (v, false))).flatMap {
        case (vs, s) ⇒
          vs.map(rv).collectFirst {
            case Some(v) ⇒ v
          }.map(FilterBound(_, s))
      }
    val l = readOne(leftQueryOp)
    val r = readOne(rightQueryOp)
    if (l.isDefined || r.isDefined) Some(FilterBounds[T](left = l, right = r)) else None
  }

  implicit def unmarshaller[T] = Unmarshaller[String, Option[FilterBounds[T]]](e => unmarshallInput[T])

  private def unmarshallInput[T](input: String)(implicit un: Unmarshaller[String, Option[T]]): Future[Option[FilterBounds[T]]] =
    if (checkFormat(input)) {
      Future.successful(None)
    } else {
      val leftStrict = input.head == leftSigns.head
      val rightStrict = input.last == rightSigns.head

      val strings = input.tail.dropRight(1).split(separator).toList

      if (strings.size > 2) {
        Future.successful(None)
      } else {
        val values = strings.map(Unmarshal(_).to[Option[T]])
        for {
          left <- values.head
          leftBound = left.map(l => FilterBound(edge = l, strict = leftStrict))
          right <- values.last
          rightBound = right.map(r => FilterBound(edge = r, strict = rightStrict))
        } yield Option(FilterBounds(leftBound, rightBound))
      }
    }


  private def checkFormat(input: String): Boolean = {
    //minimum is (;)
    if (input.size < 4) {
      false
    } else {
      leftSigns.contains(input.head) && rightSigns.contains(input.last) && input.contains(separator)
    }
  }
}

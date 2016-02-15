package utils.http.protocol

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
}

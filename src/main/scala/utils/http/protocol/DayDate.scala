package utils.http.protocol

import akka.http.scaladsl.unmarshalling.Unmarshaller
import org.joda.time.DateTime

import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.Try

case class DayDate(day: Int, month: Int, year: Int) {
  override def toString: String = s"$year-${if (month < 10) "0" + month else month}-${if (day < 10) "0" + day else day}"
}

object DayDate {
  val dateR = "([12][09][0-9][0-9])-([0-9][0-9])-([0-9][0-9])".r

  def read(s: String): Option[DayDate] = s match {
    case dateR(y, m, d) ⇒
      Try(DayDate(day = d.toInt, month = m.toInt, year = y.toInt)).toOption
    case _ ⇒
      None
  }

  implicit def toJoda(dd: DayDate): DateTime = new DateTime(dd.year, dd.month, dd.day, 0, 0)

  implicit def fromJoda(dt: DateTime): DayDate = DayDate(year = dt.getYear, month = dt.getMonthOfYear, day = dt.getDayOfMonth)

  implicit val unmarshaller = Unmarshaller[String, Option[DayDate]](e => input => Future.successful(read(input)))
}
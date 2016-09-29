package utils.http.protocol

import java.time._

import akka.http.scaladsl.unmarshalling.{ FromStringUnmarshaller, Unmarshaller }
import org.joda.time.DateTime

import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.Try

@deprecated("Use LocalDate instead", "29.09.2016")
case class DayDate(day: Int, month: Int, year: Int) {
  override def toString: String = s"$year-${if (month < 10) "0" + month else month}-${if (day < 10) "0" + day else day}"
}

@deprecated("Use LocalDate instead", "29.09.2016")
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

  implicit def toInstant(dd: DayDate)(implicit zoneId: ZoneId = ZoneOffset.UTC): Instant = ZonedDateTime.of(dd.year, dd.month, dd.day, 0, 0, 0, 0, zoneId).toInstant

  implicit def fromInstant(i: Instant)(implicit zoneId: ZoneId = ZoneOffset.UTC): DayDate = {
    val z = i.atZone(zoneId)
    DayDate(year = z.getYear, month = z.getMonth.getValue, day = z.getDayOfMonth)
  }

  implicit val unmarshaller: FromStringUnmarshaller[DayDate] =
    Unmarshaller[String, DayDate](e ⇒ input ⇒ read(input).fold[Future[DayDate]](Future.failed(ApiError.MalformedDataError))(Future.successful))
}
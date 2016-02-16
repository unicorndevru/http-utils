package utils.http.unmarshallers

import akka.actor.ActorSystem
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{ BeforeAndAfterAll, FreeSpec, Matchers }
import utils.http.protocol.{ ApiError, FilterBound, DayDate, FilterBounds }

class BoundsUnmarshallerSpec extends FreeSpec with Matchers with BeforeAndAfterAll with ScalaFutures {

  implicit val system = ActorSystem(getClass.getSimpleName)
  implicit val materializer = ActorMaterializer()

  import system.dispatcher
  import utils.http.protocol.FilterBounds._

  "Unmarshall simple DayDate value if it is correct." in {
    Unmarshal("2012-05-08").to[DayDate].futureValue shouldBe DayDate(year = 2012, month = 5, day = 8)
    Unmarshal("3012-05-08").to[DayDate].failed.futureValue should be(ApiError.MalformedDataError)

  }

  "Unmarshall correct non strict FilterBounds[DayDate]" in {
    Unmarshal("(2012-05-08;2012-05-09)").to[FilterBounds[DayDate]].futureValue shouldBe FilterBounds(
      left = Some(FilterBound(DayDate(8, 5, 2012), strict = false)),
      right = Some(FilterBound(DayDate(9, 5, 2012), strict = false))
    )
  }

  "Unmarshall correct strict FilterBounds[DayDate]" in {
    Unmarshal("[2012-05-08;2012-05-09]").to[FilterBounds[DayDate]].futureValue shouldBe FilterBounds(
      left = Some(FilterBound(DayDate(8, 5, 2012), strict = true)),
      right = Some(FilterBound(DayDate(9, 5, 2012), strict = true))
    )
  }

  "Unmarshall correct mix of strict and non strict FilterBounds[DayDate]" in {
    Unmarshal("[2012-05-08;2012-05-09)").to[FilterBounds[DayDate]].futureValue shouldBe FilterBounds(
      left = Some(FilterBound(DayDate(8, 5, 2012), strict = true)),
      right = Some(FilterBound(DayDate(9, 5, 2012), strict = false))
    )

    Unmarshal("(2012-05-08;2012-05-09]").to[FilterBounds[DayDate]].futureValue shouldBe FilterBounds(
      left = Some(FilterBound(DayDate(8, 5, 2012), strict = false)),
      right = Some(FilterBound(DayDate(9, 5, 2012), strict = true))
    )
  }

  "Don't unmarshall incorrect FilterBounds[DayDate]" in {
    Unmarshal("(;)").to[FilterBounds[DayDate]].failed.futureValue should be(ApiError.MalformedDataError)
    Unmarshal("[").to[FilterBounds[DayDate]].failed.futureValue should be(ApiError.MalformedDataError)
    Unmarshal(";").to[FilterBounds[DayDate]].failed.futureValue should be(ApiError.MalformedDataError)
    Unmarshal("2012-05-08").to[FilterBounds[DayDate]].failed.futureValue should be(ApiError.MalformedDataError)
    Unmarshal("{2012-05-08;2012-05-09}").to[FilterBounds[DayDate]].failed.futureValue should be(ApiError.MalformedDataError)
    Unmarshal("2012-05-08;2012-05-09").to[FilterBounds[DayDate]].failed.futureValue should be(ApiError.MalformedDataError)
    Unmarshal("2012-05-08:2012-05-09]").to[FilterBounds[DayDate]].failed.futureValue should be(ApiError.MalformedDataError)
    Unmarshal("[4412-05-08;2012-05-09]").to[FilterBounds[DayDate]].failed.futureValue should be(ApiError.MalformedDataError)
    Unmarshal("[xuiv-am-08;2012-05-09]").to[FilterBounds[DayDate]].failed.futureValue should be(ApiError.MalformedDataError)
    Unmarshal("[2012-05-08;4412-05-09]").to[FilterBounds[DayDate]].failed.futureValue should be(ApiError.MalformedDataError)
  }

  override def afterAll() = system.shutdown()
}

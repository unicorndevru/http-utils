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

  "simple should be simple" in {
    Unmarshal("(;)").to[DayDate].failed.futureValue should be(ApiError.MalformedDataError)
    Unmarshal("2012-05-08").to[DayDate].futureValue shouldBe DayDate(year = 2012, month = 5, day = 8)
  }

  "daydate should be unmarshalled correctly" in {
    Unmarshal("(2012-05-08;2012-05-09)").to[FilterBounds[DayDate]].futureValue shouldBe FilterBounds(
      left = Some(FilterBound(DayDate(8, 5, 2012), strict = false)),
      right = Some(FilterBound(DayDate(9, 5, 2012), strict = false))
    )
  }

  override def afterAll() = system.shutdown()
}

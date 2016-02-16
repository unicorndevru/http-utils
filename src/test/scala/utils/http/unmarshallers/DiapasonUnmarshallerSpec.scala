package utils.http.unmarshallers

import akka.actor.ActorSystem
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{ BeforeAndAfterAll, FreeSpec, Matchers }
import utils.http.protocol.{ ApiError, DiapasonSide, DayDate, Diapason }

class DiapasonUnmarshallerSpec extends FreeSpec with Matchers with BeforeAndAfterAll with ScalaFutures {

  implicit val system = ActorSystem(getClass.getSimpleName)
  implicit val materializer = ActorMaterializer()

  import system.dispatcher
  import utils.http.protocol.Diapason._

  "Unmarshall simple DayDate value if it is correct." in {
    Unmarshal("2012-05-08").to[DayDate].futureValue shouldBe DayDate(year = 2012, month = 5, day = 8)
    Unmarshal("3012-05-08").to[DayDate].failed.futureValue should be(ApiError.MalformedDataError)
  }

  "Unmarshall correct non strict Diapason[DayDate]" in {
    Unmarshal("(2012-05-08;2012-05-09)").to[Diapason[DayDate]].futureValue shouldBe Diapason(
      from = Some(DiapasonSide(DayDate(8, 5, 2012), inclusive = false)),
      to = Some(DiapasonSide(DayDate(9, 5, 2012), inclusive = false))
    )
  }

  "Unmarshall correct strict Diapason[DayDate]" in {
    Unmarshal("[2012-05-08;2012-05-09]").to[Diapason[DayDate]].futureValue shouldBe Diapason(
      from = Some(DiapasonSide(DayDate(8, 5, 2012), inclusive = true)),
      to = Some(DiapasonSide(DayDate(9, 5, 2012), inclusive = true))
    )
  }

  "Unmarshall correct mix of strict and non strict Diapason[DayDate]" in {
    Unmarshal("[2012-05-08;2012-05-09)").to[Diapason[DayDate]].futureValue shouldBe Diapason(
      from = Some(DiapasonSide(DayDate(8, 5, 2012), inclusive = true)),
      to = Some(DiapasonSide(DayDate(9, 5, 2012), inclusive = false))
    )

    Unmarshal("(2012-05-08;2012-05-09]").to[Diapason[DayDate]].futureValue shouldBe Diapason(
      from = Some(DiapasonSide(DayDate(8, 5, 2012), inclusive = false)),
      to = Some(DiapasonSide(DayDate(9, 5, 2012), inclusive = true))
    )
  }

  "Unmarshall other types" in {
    Unmarshal("(0;1]").to[Diapason[Int]].futureValue shouldBe Diapason(
      from = Some(DiapasonSide(0, inclusive = false)),
      to = Some(DiapasonSide(1, inclusive = true))
    )
  }

  "Don't unmarshall incorrect Diapason[DayDate]" in {
    Unmarshal("(;)").to[Diapason[DayDate]].failed.futureValue should be(ApiError.MalformedDataError)
    Unmarshal("[").to[Diapason[DayDate]].failed.futureValue should be(ApiError.MalformedDataError)
    Unmarshal(";").to[Diapason[DayDate]].failed.futureValue should be(ApiError.MalformedDataError)
    Unmarshal("2012-05-08").to[Diapason[DayDate]].failed.futureValue should be(ApiError.MalformedDataError)
    Unmarshal("{2012-05-08;2012-05-09}").to[Diapason[DayDate]].failed.futureValue should be(ApiError.MalformedDataError)
    Unmarshal("2012-05-08;2012-05-09").to[Diapason[DayDate]].failed.futureValue should be(ApiError.MalformedDataError)
    Unmarshal("2012-05-08:2012-05-09]").to[Diapason[DayDate]].failed.futureValue should be(ApiError.MalformedDataError)
    Unmarshal("[4412-05-08;2012-05-09]").to[Diapason[DayDate]].failed.futureValue should be(ApiError.MalformedDataError)
    Unmarshal("[xuiv-am-08;2012-05-09]").to[Diapason[DayDate]].failed.futureValue should be(ApiError.MalformedDataError)
    Unmarshal("[2012-05-08;4412-05-09]").to[Diapason[DayDate]].failed.futureValue should be(ApiError.MalformedDataError)
  }

  override def afterAll() = {
    system.shutdown()
    super.afterAll()
  }
}

package utils.http.unmarshallers

import akka.actor.ActorSystem
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers}
import utils.http.protocol.{FilterBound, DayDate}

class BoundsUnmarshallerSpec extends FreeSpec with Matchers with BeforeAndAfterAll with ScalaFutures {

  implicit val system = ActorSystem(getClass.getSimpleName)
  implicit val materializer = ActorMaterializer()
  import system.dispatcher
  import utils.http.protocol.FilterBounds._

  "simple should be simple" in {
    Unmarshal("(;)").to[Option[DayDate]].futureValue shouldBe None
  }

  "daydate should be unmarshalled correctly" in {
    Unmarshal("(2012-05-08;2012-05-09)").to[FilterBound[DayDate]].futureValue shouldBe defined
  }

  override def afterAll() = system.shutdown()
}

package utils.http.json

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.RequestEntity
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{ Seconds, Span }
import play.api.libs.json.Json
import org.scalatest.{ BeforeAndAfterAll, Matchers, WordSpec }

class PlayJsonSupportSpec extends WordSpec with Matchers with BeforeAndAfterAll with ScalaFutures {
  case class Foo(bar: String)
  import PlayJsonSupport._

  implicit val fooFormat = Json.format[Foo]

  implicit val system = ActorSystem(getClass.getSimpleName)
  implicit val mat = ActorMaterializer()

  implicit val pc = PatienceConfig(Span(1, Seconds))

  "PlayJsonSupport" should {
    import system.dispatcher

    "enable marshalling and unmarshalling objects for which `Writes` and `Reads` exist" in {
      val foo = Foo("bar")

      val entity = Marshal(foo).to[RequestEntity].futureValue
      Unmarshal(entity).to[Foo].futureValue shouldBe foo
    }
  }

  override protected def afterAll() = {
    system.shutdown()
    super.afterAll()
  }
}

package utils.http.json

import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.{ StatusCodes, RequestEntity }
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{ Seconds, Span }
import play.api.libs.json.{ Format, JsObject, Json }
import org.scalatest.{ BeforeAndAfterAll, Matchers, WordSpec }
import utils.http.ApiErrorHandler

class PlayJsonSupportSpec extends WordSpec with Matchers with BeforeAndAfterAll with ScalaFutures with ScalatestRouteTest with PlayJsonSupport {
  case class Foo(bar: String)

  implicit val fooFormat: Format[Foo] = Json.format[Foo]

  implicit val mat = ActorMaterializer()

  implicit val pc = PatienceConfig(Span(1, Seconds))

  implicit val rh = ApiErrorHandler.rejectionHandler
  implicit val eh = ApiErrorHandler.exceptionHandler

  lazy val route = Route.seal(entity(as[Foo]){ e ⇒ complete(e) })

  "PlayJsonSupport" should {

    "enable marshalling and unmarshalling objects for which `Writes` and `Reads` exist" in {
      val foo = Foo("bar")

      val entity = Marshal(foo).to[RequestEntity].futureValue
      Unmarshal(entity).to[Foo].futureValue shouldBe foo
    }

    "return correct 400 error on malformed request" in {
      Post("Any", Json.toJson(Foo("bar"))) ~> route ~> check {
        status should be(StatusCodes.OK)
        entityAs[Foo] should be(Foo("bar"))
      }

      Post("any", Json.obj("other" → "thing")) ~> route ~> check {
        status should be(StatusCodes.BadRequest)
        entityAs[JsObject] should be(Json.parse(
          """{"code":"validation.error","desc":"Validation Error","fields":{"bar":{"code":"validation.json","desc":"error.path.missing"}}} """
        ))
      }
    }
  }
}

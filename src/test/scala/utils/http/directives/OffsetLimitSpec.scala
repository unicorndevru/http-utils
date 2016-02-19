package utils.http.directives

import akka.http.scaladsl.server.{ MalformedQueryParamRejection, Directives }
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{ Inside, Matchers, WordSpec }

class OffsetLimitSpec extends WordSpec with Matchers with Directives with ScalatestRouteTest with OffsetLimitDirectives with Inside {

  "Th OffsetLimit directive" should {
    "Use default values correctly" in {
      Get("/abc") ~>
        { offsetLimit() { (x, y) ⇒ complete(s"$x, $y") } } ~>
        check { responseAs[String] shouldEqual "0, 16" }
    }

    "Parse correct values" in {
      Get("/abc?limit=17&offset=5") ~>
        { offsetLimit() { (x, y) ⇒ complete(s"$x, $y") } } ~>
        check { responseAs[String] shouldEqual "5, 17" }
    }

    "Use default values instead of missed params" in {
      Get("/abc?offset=5") ~>
        { offsetLimit() { (x, y) ⇒ complete(s"$x, $y") } } ~>
        check { responseAs[String] shouldEqual "5, 16" }
      Get("/abc?limit=5") ~>
        { offsetLimit() { (x, y) ⇒ complete(s"$x, $y") } } ~>
        check { responseAs[String] shouldEqual "0, 5" }
    }

    "Doesn't parse incorrect values" in {
      Get("/abc?limit=aa&offset=cc") ~>
        { offsetLimit() { (x, y) ⇒ complete(s"$x, $y") } } ~>
        check {
          inside(rejection) {
            case MalformedQueryParamRejection("offset", "'cc' is not a valid 32-bit signed integer value", Some(_)) ⇒
          }
        }
    }

    "Limit max to 50 by default" in {
      Get("/abc") ~>
        { offsetLimit(100) { (x, y) ⇒ complete(s"$x, $y") } } ~>
        check { responseAs[String] shouldEqual "0, 50" }
    }

    "Use non default limit correctly" in {
      Get("/abc") ~>
        { offsetLimit(100, 150) { (x, y) ⇒ complete(s"$x, $y") } } ~>
        check { responseAs[String] shouldEqual "0, 100" }

      Get("/abc") ~>
        { offsetLimit(100, 90) { (x, y) ⇒ complete(s"$x, $y") } } ~>
        check { responseAs[String] shouldEqual "0, 90" }
    }
  }
}

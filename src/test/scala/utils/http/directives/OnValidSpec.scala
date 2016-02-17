package utils.http.directives

import akka.http.scaladsl.server.{Directives, Route}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{Inside, Matchers, WordSpec}
import utils.http.ApiErrorHandler
import utils.http.protocol.ApiError

import scala.concurrent.Future

class OnValidSpec extends WordSpec with Matchers with Directives with ScalatestRouteTest with ValidationDirectives with Inside{

  implicit val exceptionHandler = ApiErrorHandler.generic

  val happyPath = Route.seal({ onValid(true) { complete("ok")}})
  val defaultUnhappyPath = Route.seal({ onValid(false) { complete("ok")}})
  val customUnhappyPath = Route.seal({ onValid(false -> new ApiError("some","some")) { complete("ok")}})
  val futureUnhappyPath = Route.seal({ onValid(Future.failed(new ApiError("some2","some2"))) { complete("ok")}})
  val futureUnhappyPathWithField = Route.seal({ onValid("fieldName" -> Future.failed(new ApiError("some3","some3"))) { complete("ok")}})

  "The onValid directive" should {
    "accept true values" in {
      Get("/abc") ~> happyPath ~> check { responseAs[String] shouldEqual "ok" }
    }

    "doesn't accept false values" in {
      Get("/abc") ~> defaultUnhappyPath ~>
        check { responseAs[String] shouldEqual
          """{
            |  "code" : "validation.error",
            |  "desc" : "Validation Error",
            |  "fields" : { }
            |}""".stripMargin }
    }

    "send custom errors" in {
      Get("/abc") ~> customUnhappyPath ~>
        check { responseAs[String] shouldEqual
          """{
            |  "code" : "some",
            |  "desc" : "some"
            |}""".stripMargin}
    }

    "fails if future fails" in {
      Get("/abc") ~> futureUnhappyPath ~>
        check { responseAs[String] shouldEqual
          """{
            |  "code" : "some2",
            |  "desc" : "some2"
            |}""".stripMargin}
    }

    "fails for field if future fails" in {
      Get("/abc") ~> futureUnhappyPathWithField ~>
        check { responseAs[String] shouldEqual
          """{
            |  "code" : "validation.error",
            |  "desc" : "Validation Error",
            |  "fields" : {
            |    "fieldName" : {
            |      "code" : "some3",
            |      "desc" : "some3"
            |    }
            |  }
            |}""".stripMargin}
    }
  }
}

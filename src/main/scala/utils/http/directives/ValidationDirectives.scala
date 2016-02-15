package utils.http.directives

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import utils.http.protocol.{ ApiError, ValidationError }

import scala.concurrent.Future
import scala.language.implicitConversions
import scala.concurrent.ExecutionContext.Implicits.global

trait ValidationDirectives {
  sealed trait ValidityCheck {
    def check: Future[_]
  }

  private val validCheck: ValidityCheck = new ValidityCheck {
    override def check = Future.successful(())
  }

  implicit def boolToValidityCheck(b: Boolean): ValidityCheck =
    (b, ValidationError)

  implicit def boolWithFailureToValidityCheck(bf: (Boolean, Throwable)): ValidityCheck = new ValidityCheck {
    override def check = if (bf._1) Future.successful(true) else Future.failed(bf._2)
  }

  implicit def futureToValidityCheck(f: Future[_]): ValidityCheck = new ValidityCheck {
    override def check = f
  }

  implicit def fieldFutureToValidityCheck(nf: (String, Future[_])): ValidityCheck = nf._2.recoverWith {
    case t: ValidationError ⇒ Future.failed(t)
    case t: ApiError        ⇒ Future.failed(t.forField(nf._1))
  }

  implicit def optValidityCheck[T](t: Option[T])(implicit c: T ⇒ ValidityCheck): ValidityCheck =
    t.fold(validCheck)(c)

  /**
   * Applicative validation: collects errors from all validity checks, combines them into single ValidationError error, if possible
   *
   * @param checks either boolean, boolean -> errorToThrow, Future[Any], fieldName -> Future[Any]
   * @return
   */
  def onValid(checks: ValidityCheck*): Directive0 = {
    onSuccess(Future.traverse(checks)(_.check.map(_ ⇒ None).recover {
      case t: Throwable ⇒ Some(t)
    })).flatMap { cs ⇒
      val failures = cs.flatten
      if (failures.isEmpty) {
        pass
      } else if (failures.forall {
        case t: ValidationError ⇒ true
        case _                  ⇒ false
      }) {

        failWith(failures.collect {
          case t: ValidationError ⇒ t
        }.reduce(_ combine _))

      } else if (failures.size == 1) {

        failWith(failures.head)

      } else {

        failWith(ValidationError)

      }
    }
  }
}

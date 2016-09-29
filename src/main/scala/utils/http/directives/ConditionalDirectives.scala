package utils.http.directives

import java.util.UUID

import akka.http.scaladsl.model.DateTime
import akka.http.scaladsl.model.headers.CacheDirectives.{ `max-age`, `must-revalidate` }
import akka.http.scaladsl.model.headers.{ EntityTag, `Cache-Control` }
import akka.http.scaladsl.server._
import utils.http.protocol.UpdatedAt

import scala.concurrent.Future

trait ConditionalDirectives {
  def userStringIdAware: Directive1[Option[String]]

  import Directives._

  private val zeroUUID = new UUID(0, 0).toString

  private val forceRevalidate = mapResponseHeaders(_ :+ `Cache-Control`(`max-age`(0), `must-revalidate`))

  def lastUpdated[T: UpdatedAt](lm: T, mustRevalidate: Boolean = true): Directive0 =
    userStringIdAware.flatMap {
      uao ⇒
        val tag = uao.getOrElse(zeroUUID)
        val milli = implicitly[UpdatedAt[T]].apply(lm).toEpochMilli
        conditional(EntityTag(tag + milli), DateTime(milli)).tflatMap(_ ⇒
          if (mustRevalidate) forceRevalidate else pass)
    }

  def lastUpdatedF[T: UpdatedAt](lm: Future[T], mustRevalidate: Boolean = true): Directive1[T] =
    onSuccess(lm).flatMap(v ⇒ lastUpdated(v, mustRevalidate).tflatMap(_ ⇒ provide(v)))

  private def md = java.security.MessageDigest.getInstance("MD5")

  private def md5Hex(str: String) = {
    md.digest(str.getBytes).map("%02x" format _).mkString
  }

  def hashETag(mustRevalidate: Boolean, hashes: String*) =
    userStringIdAware.flatMap {
      uao ⇒
        val userPart = uao.getOrElse(zeroUUID)
        val content = userPart + hashes.mkString
        val hash = md5Hex(content)
        conditional(EntityTag(hash)).tflatMap(_ ⇒
          if (mustRevalidate) forceRevalidate else pass)
    }
}

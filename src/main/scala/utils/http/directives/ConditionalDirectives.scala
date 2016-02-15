package utils.http.directives

import java.util.UUID

import akka.http.scaladsl.model.DateTime
import akka.http.scaladsl.model.headers.EntityTag
import akka.http.scaladsl.server._
import utils.http.protocol.LastUpdated

import scala.concurrent.Future

trait ConditionalDirectives {
  def userStringIdAware: Directive1[Option[String]]

  import Directives._

  private val zeroUUID = new UUID(0, 0).toString

  def lastUpdated(lm: LastUpdated): Directive0 =
    userStringIdAware.flatMap {
      uao ⇒
        val tag = uao.getOrElse(zeroUUID)
        conditional(EntityTag(tag), DateTime(lm.lastUpdated.getMillis))
    }

  def lastUpdated[T <: LastUpdated](lm: Future[T]): Directive1[T] =
    onSuccess(lm).flatMap(v ⇒ lastUpdated(v).tflatMap(_ ⇒ provide(v)))

  private def md = java.security.MessageDigest.getInstance("MD5")

  private def md5Hex(str: String) = {
    md.digest(str.getBytes).map("%02x" format _).mkString
  }

  def hashETag(hashes: String*) =
    userStringIdAware.flatMap {
      uao ⇒
        val userPart = uao.getOrElse(zeroUUID)
        val content = userPart + hashes.mkString
        val hash = md5Hex(content)
        conditional(EntityTag(hash))
    }
}

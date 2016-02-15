package utils.http.directives

import akka.http.scaladsl.server.Directive
import akka.http.scaladsl.server.Directives._

trait OffsetLimitDirectives {
  def offsetLimit(defaultLimit: Int = 16, max: Int = 50): Directive[(Int, Int)] =
    parameters('offset.as[Int] ? 0, 'limit.as[Int] ? defaultLimit).tflatMap { ol ⇒
      tprovide(ol.copy(_2 = Math.min(ol._2, max)))
    }
}

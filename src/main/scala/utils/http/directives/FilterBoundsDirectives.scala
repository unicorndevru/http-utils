package utils.http.directives

import akka.http.scaladsl.server.Directives._
import utils.http.protocol.{ DayDate, FilterBounds }

trait FilterBoundsDirectives {

  def bounds[T](name: String, rv: String ⇒ Option[T]) =
    parameterMultiMap.flatMap { m ⇒
      provide(FilterBounds.parseQueryParams(name, m, rv))
    }

  def dayDateBounds(name: String) = bounds(name, DayDate.read)

}

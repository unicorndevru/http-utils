package utils.http.protocol

import java.time.Instant

@deprecated("Use UpdatedAt typeclass instead", "29.09.2016")
trait LastUpdated {
  def lastUpdated: Instant
}

@deprecated("Use UpdatedAt typeclass instead", "29.09.2016")
object LastUpdated {
  implicit def updatedAtLastUpdated[T <: LastUpdated] = UpdatedAt[T](_.lastUpdated)
}

case class UpdatedAt[T](updatedAt: T ⇒ Instant) {
  def apply(v: T) = updatedAt(v)
}
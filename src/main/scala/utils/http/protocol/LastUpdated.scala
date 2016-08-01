package utils.http.protocol

import java.time.Instant

trait LastUpdated {
  def lastUpdated: Instant
}

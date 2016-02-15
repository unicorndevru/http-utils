package utils.http.protocol

import org.joda.time.DateTime

trait LastUpdated {
  def lastUpdated: DateTime
}

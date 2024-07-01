package io.github.dataunitylab.jsonoid.discovery

/** Represents different levels of anomaly severity. */
sealed trait AnomalyLevel extends Ordered[AnomalyLevel] {
  val order: Int

  def compare(that: AnomalyLevel): Int = this.order - that.order
}

object AnomalyLevel {

  /** This anomaly is purely for informational purposes. */
  case object Info extends AnomalyLevel {
    override val order: Int = 0
  }

  /** This is an anomaly for a value which is structurally valid, but falls
    * outside of the expected range.
    */
  case object Warning extends AnomalyLevel {
    override val order: Int = 1
  }

  /** This is an anomaly which would cause the value to fail structural
    * validation.
    */
  case object Fatal extends AnomalyLevel {
    override val order: Int = 2
  }
}

/** Anomalies are potential violations of a schema according to a given value.
  *
  * @param path
  *   the path where this anomaly occurred
  * @param message
  *   a message describing this anomaly
  * @param anomalyLevel
  *   the level of this anomaly
  */
final case class Anomaly(
    path: String,
    message: String,
    anomalyLevel: AnomalyLevel
) {}

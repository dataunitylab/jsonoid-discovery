package edu.rit.cs.mmior.jsonoid.discovery

/** Represents different levels of anomaly severity. */
sealed trait AnomalyLevel {}

/** This anomaly is purely for informational purposes. */
case object Info extends AnomalyLevel {}

/** This is an anomaly for a value which is structurally valid, but falls
  *  outside of the expected range.
  */
case object Warning extends AnomalyLevel {}

/** This is an anomaly which would cause the value to fail structural validation. */
case object Fatal extends AnomalyLevel {}

/** Anomalies are potential violations of a schema according to a given value.
  *
  * @param path the path where this anomaly occurred
  * @param message a message describing this anomaly
  * @param anomalyLevel the level of this anomaly
  */
final case class Anomaly(
    path: String,
    message: String,
    anomalyLevel: AnomalyLevel
) {}

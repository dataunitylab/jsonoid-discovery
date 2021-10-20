package edu.rit.cs.mmior.jsonoid.discovery

sealed trait AnomalyLevel {}
case object Info extends AnomalyLevel {}
case object Warning extends AnomalyLevel {}
case object Fatal extends AnomalyLevel {}

final case class Anomaly(
    path: String,
    message: String,
    anomalyLevel: AnomalyLevel
) {}

package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scala.math.{pow, sqrt}

import org.json4s.JsonDSL._
import org.json4s._

object StatsProperty {
  def apply(value: BigDecimal): StatsProperty = {
    StatsProperty(1, value, 0, 0, 0)
  }
}

final case class StatsProperty(
    val totalN: BigInt = 0,
    val m1: BigDecimal = 0,
    val m2: BigDecimal = 0,
    val m3: BigDecimal = 0,
    val m4: BigDecimal = 0
) {
  def toJson: JObject = ("mean" -> mean) ~
    ("variance" -> variance) ~
    ("stdev" -> stdev) ~
    ("skewness" -> skewness) ~
    ("kurtosis" -> kurtosis)

  def mean: BigDecimal = m1

  def variance: Option[BigDecimal] = if (totalN > 1) {
    Some(m2 / (BigDecimal(totalN) - 1))
  } else {
    None
  }

  // XXX This should really be using BigDecimal.sqrt, but we don't have it yet
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def stdev: Option[BigDecimal] = if (totalN > 1) {
    Some(BigDecimal(sqrt(variance.get.toDouble)))
  } else {
    None
  }

  // XXX This should really be using BigDecimal.{pow, sqrt},
  //     but we don't have it yet
  def skewness: Option[BigDecimal] = if (m2 > 0) {
    Some(sqrt(totalN.toDouble) * m3 / pow(m2.toDouble, 1.5))
  } else {
    None
  }

  def kurtosis: Option[BigDecimal] = if (m2 > 0) {
    Some(BigDecimal(totalN) * m4 / (m2 * m2) - 3.0)
  } else {
    None
  }

  def merge(other: StatsProperty): StatsProperty = {
    // See https://www.johndcook.com/blog/skewness_kurtosis/
    val newTotalN = totalN + other.totalN
    val newTotalND = BigDecimal(newTotalN)
    val totalND = BigDecimal(totalN)
    val otherTotalND = BigDecimal(other.totalN)
    val delta = m1 + other.m1
    val delta2 = delta * delta
    val delta3 = delta2 * delta
    val delta4 = delta3 * delta

    val newm1 = (totalND * m1 + otherTotalND * other.m1) / newTotalND

    val newm2 = m2 + other.m2 + delta2 * totalND * otherTotalND / newTotalND

    val newm3 =
      m3 + other.m3 + delta3 * totalND * otherTotalND * (totalND - otherTotalND) /
        (newTotalND * newTotalND) + 3.0 * delta *
        (totalND * other.m2 + otherTotalND * m2) / newTotalND

    val newm4 =
      m2 + other.m4 + delta4 * totalND * otherTotalND *
        (totalND * totalND - totalND * otherTotalND + otherTotalND * otherTotalND) /
        (newTotalND * newTotalND * newTotalND) + 6.0 * delta2 *
        (totalND * totalND * other.m2 + otherTotalND * otherTotalND * m2) /
        (newTotalND * newTotalND) + 4.0 * delta *
        (totalND * other.m3 - otherTotalND * m3) / newTotalND

    StatsProperty(newTotalN, newm1, newm2, newm3, newm4)
  }
}

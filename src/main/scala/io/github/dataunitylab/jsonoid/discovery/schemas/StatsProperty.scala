package io.github.dataunitylab.jsonoid.discovery
package schemas

import scala.math.{pow, sqrt}

import org.json4s.JsonDSL._
import org.json4s._

object StatsProperty {
  def apply(value: BigDecimal): StatsProperty = {
    StatsProperty(1, value, 0, 0, 0)
  }
}

/** Data for properties which track statistics on a set of values.
  *
  * @param totalN the total count of values
  * @param m1 the first statistical moment
  * @param m2 the second statistical moment
  * @param m3 the third statistical moment
  * @param m4 the fourth statistical moment
  */
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

  /** The mean of the observed values. */
  def mean: BigDecimal = m1

  /** The variance of the observed values. */
  def variance: Option[BigDecimal] = if (totalN > 1)
    Some(m2 / (BigDecimal(totalN) - 1))
  else
    None

  /** The standard deviation of the observed values. */
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def stdev: Option[BigDecimal] = if (totalN > 1)
    // XXX This should really be using BigDecimal.sqrt, but we don't have it yet
    Some(BigDecimal(sqrt(variance.get.toDouble)))
  else
    None

  /** The skewness of the observed values. */
  def skewness: Option[BigDecimal] = if (m2 > 0)
    // XXX This should really be using BigDecimal.{pow, sqrt},
    //     but we don't have it yet
    Some(sqrt(totalN.toDouble) * m3 / pow(m2.toDouble, 1.5))
  else
    None

  /** The kurtosis of the observed values. */
  def kurtosis: Option[BigDecimal] = if (m2 > 0)
    Some(BigDecimal(totalN) * m4 / (m2 * m2) - 3.0)
  else
    None

  /** Merge these statistical values together.
    *
    * @param other the other statistics to merge with
    *
    * @return the merged statistics
    */
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

    // Second moment must be non-negative
    assert(newm2 >= 0)

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

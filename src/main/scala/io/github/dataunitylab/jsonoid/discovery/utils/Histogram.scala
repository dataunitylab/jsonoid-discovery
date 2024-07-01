package io.github.dataunitylab.jsonoid.discovery
package utils

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ListBuffer

import com.datadoghq.sketch.ddsketch.{DDSketch, DDSketches}
import com.datadoghq.sketch.ddsketch.store.Bin
import org.json4s.JsonDSL._
import org.json4s._

import Helpers._

object Histogram {

  /** Default error tolerance of the histogram. */
  val Tolerance: Double = 0.01
}

/** A histogram of the values in a given set.:w
  *
  * @constructor
  *   Create a new histogram
  * @param sketch
  *   the sketch to use for the histogram
  */
final case class Histogram(
    sketch: DDSketch = DDSketches.unboundedDense(Histogram.Tolerance),
    hasExtremeValues: Boolean = false
) {

  private def zeroCount: Int = {
    val zeros = (sketch.getCount - (sketch.getNegativeValueStore.getTotalCount +
      sketch.getPositiveValueStore.getTotalCount)).toInt

    // There must be a non-negative number of zero values
    assert(zeros >= 0)

    zeros
  }

  def toJson: JObject = {
    ("bins" -> bins.map { case (value, count) =>
      List(JDouble(value.doubleValue), JLong(count.longValue))
    }) ~ ("hasExtremeValues" -> hasExtremeValues)
  }

  /** Produce bins for the histogram.
    *
    * @return
    *   a list of bins representing the histogram
    */
  @SuppressWarnings(
    Array(
      "org.wartremover.warts.MutableDataStructures",
      "org.wartremover.warts.NonUnitStatements"
    )
  )
  def bins: List[(Double, Int)] = {
    val indexMapping = sketch.getIndexMapping
    val bins = ListBuffer.empty[(Double, Int)]

    // Negative bins must have their values
    // inverted and we go in descending order
    sketch.getNegativeValueStore.getDescendingIterator.asScala.foreach {
      bin: Bin =>
        bins += ((-indexMapping.value(bin.getIndex), bin.getCount.toInt))
    }

    // Add any zero values
    val zeroCountInt = zeroCount
    if (zeroCountInt > 0) {
      bins += ((0.0, zeroCountInt))
    }

    // Add positive values
    sketch.getPositiveValueStore.getAscendingIterator.asScala.foreach {
      bin: Bin =>
        bins += ((indexMapping.value(bin.getIndex), bin.getCount.toInt))
    }

    // The bins must be in ascending order
    assert(bins.map(_._1).sorted === bins.map(_._1))

    bins.toList
  }

  /** Check if a given value is trackable by this histogram
    *
    * @param value
    *   the value to check
    */
  def isTrackable(value: Double): Boolean = {
    val indexMapping = sketch.getIndexMapping
    // XXX We may lose some precision here, but we assume
    //     that we can track arbitrarily small values
    value.abs < indexMapping.maxIndexableValue
  }

  /** Merge this histogram with another histogram.
    *
    * @param other
    *   the histogram to merge with
    *
    * @return
    *   the merged histogram
    */
  def merge(other: Histogram): Histogram = {
    val newHistogram =
      Histogram(hasExtremeValues = hasExtremeValues || other.hasExtremeValues)
    newHistogram.sketch.mergeWith(sketch)
    newHistogram.sketch.mergeWith(other.sketch)

    newHistogram
  }

  /** Merge a value into this histogram.
    *
    * @param value
    *   the value to merge into the histogram
    *
    * @return
    *   the merged histogram
    */
  def merge(value: Double): Histogram = {
    val trackable = isTrackable(value)
    val newHistogram = Histogram(hasExtremeValues = !trackable)
    newHistogram.sketch.mergeWith(sketch)
    if (trackable) {
      newHistogram.sketch.accept(value)
    }

    newHistogram
  }

  /** Merge a value into this histogram.
    *
    * @param value
    *   the value to merge into the histogram
    *
    * @return
    *   the merged histogram
    */
  def merge(value: BigInt): Histogram = {
    if (value.isValidLong) {
      merge(value.doubleValue)
    } else {
      Histogram(sketch, true)
    }
  }

  /** Merge a value into this histogram.
    *
    * @param value
    *   the value to merge into the histogram
    *
    * @return
    *   the merged histogram
    */
  def merge(value: BigDecimal): Histogram = {
    if (value.isInDoubleRange) {
      merge(value.doubleValue)
    } else {
      Histogram(sketch, true)
    }
  }

  /** Check if a value is anamolous according to the histogram.
    *
    * @param value
    *   the value to check for in the histogram
    *
    * @return
    *   whether the value is anomalous according to the histogram
    */
  @SuppressWarnings(Array("org.wartremover.warts.Return"))
  def isAnomalous(value: BigInt): Boolean = {
    value.isValidLong && isTrackable(value.doubleValue) && isAnomalous(
      value.doubleValue
    )
  }

  /** Check if a value is anamolous according to the histogram.
    *
    * @param value
    *   the value to check for in the histogram
    *
    * @return
    *   whether the value is anomalous according to the histogram
    */
  @SuppressWarnings(Array("org.wartremover.warts.Return"))
  def isAnomalous(value: Double): Boolean = {
    // We consider values that can't be tracked as not anomalous
    if (!isTrackable(value)) {
      return false
    }

    val mapping = sketch.getIndexMapping

    val maxValue = if (!sketch.getPositiveValueStore.isEmpty) {
      mapping.upperBound(
        sketch.getPositiveValueStore.getDescendingIterator.asScala
          .next()
          .getIndex
      )
    } else if (!sketch.getNegativeValueStore.isEmpty && zeroCount === 0) {
      -mapping.lowerBound(
        sketch.getNegativeValueStore.getAscendingIterator.asScala
          .next()
          .getIndex
      )
    } else {
      0
    }

    val minValue = if (!sketch.getNegativeValueStore.isEmpty) {
      -mapping.upperBound(
        sketch.getNegativeValueStore.getDescendingIterator.asScala
          .next()
          .getIndex
      )
    } else if (!sketch.getPositiveValueStore.isEmpty && zeroCount === 0) {
      mapping.lowerBound(
        sketch.getPositiveValueStore.getAscendingIterator.asScala
          .next()
          .getIndex
      )
    } else {
      0
    }

    // Maximum value must not be less than the minimum
    assert(maxValue >= minValue)

    value < minValue || value > maxValue
  }
}

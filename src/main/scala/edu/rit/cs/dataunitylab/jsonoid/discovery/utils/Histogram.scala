package edu.rit.cs.dataunitylab.jsonoid.discovery
package utils

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ListBuffer

import com.datadoghq.sketch.ddsketch.{DDSketch, DDSketches}
import com.datadoghq.sketch.ddsketch.store.Bin

import Helpers._

object Histogram {

  /** Default error tolerance of the histogram. */
  val Tolerance: Double = 0.01
}

/** A histogram of the values in a given set.:w
  *
  * @constructor Create a new histogram
  * @param sketch the sketch to use for the histogram
  */
final case class Histogram(
    sketch: DDSketch = DDSketches.unboundedDense(Histogram.Tolerance)
) {

  private def zeroCount: Int = {
    val zeros = (sketch.getCount - (sketch.getNegativeValueStore.getTotalCount +
      sketch.getPositiveValueStore.getTotalCount)).toInt

    // There must be a non-negative number of zero values
    assert(zeros >= 0)

    zeros
  }

  /** Produce bins for the histogram.
    *
    * @return a list of bins representing the histogram
    */
  @SuppressWarnings(
    Array(
      "org.wartremover.warts.MutableDataStructures",
      "org.wartremover.warts.NonUnitStatements"
    )
  )
  def bins(): List[(Double, Int)] = {
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

  /** Merge this histogram with another histogram.
    *
    * @param other the histogram to merge with
    *
    * @return the merged histogram
    */
  def merge(other: Histogram): Histogram = {
    val newHistogram = Histogram()
    newHistogram.sketch.mergeWith(sketch)
    newHistogram.sketch.mergeWith(other.sketch)

    newHistogram
  }

  /** Merge a value into this histogram.
    *
    * @param value the value to merge into the histogram
    *
    * @return the merged histogram
    */
  def merge(value: Double): Histogram = {
    val newHistogram = Histogram()
    newHistogram.sketch.mergeWith(sketch)
    newHistogram.sketch.accept(value)

    newHistogram
  }

  /** Check if a value is anamolous according to the histogram.
    *
    * @param value the value to check for in the histogram
    *
    * @return whether the value is anomalous according to the histogram
    */
  def isAnomalous(value: Double): Boolean = {
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

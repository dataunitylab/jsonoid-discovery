package edu.rit.cs.mmior.jsonoid.discovery
package utils

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

import com.datadoghq.sketch.ddsketch.{DDSketch, DDSketches}
import com.datadoghq.sketch.ddsketch.store.Bin;

object Histogram {
  val Tolerance: Double = 0.01
}

final case class Histogram(
    sketch: DDSketch = DDSketches.unboundedDense(Histogram.Tolerance)
) {

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
    val zeroCount =
      sketch.getCount - (sketch.getNegativeValueStore.getTotalCount +
        sketch.getPositiveValueStore.getTotalCount)
    if (zeroCount > 0) {
      bins += ((0.0, zeroCount.toInt))
    }

    // Add positive values
    sketch.getPositiveValueStore.getAscendingIterator.asScala.foreach {
      bin: Bin =>
        bins += ((indexMapping.value(bin.getIndex), bin.getCount.toInt))
    }

    bins.toList
  }

  def merge(other: Histogram): Histogram = {
    val newHistogram = Histogram()
    newHistogram.sketch.mergeWith(sketch)
    newHistogram.sketch.mergeWith(other.sketch)

    newHistogram
  }

  def merge(value: Double): Histogram = {
    val newHistogram = Histogram()
    newHistogram.sketch.mergeWith(sketch)
    newHistogram.sketch.accept(value)

    newHistogram
  }

  def isAnomalous(value: Double): Boolean = {
    val mapping = sketch.getIndexMapping
    val maxValue = if (sketch.getPositiveValueStore.isEmpty) {
      -mapping.lowerBound(
        sketch.getNegativeValueStore.getAscendingIterator.asScala.next.getIndex
      )
    } else {
      mapping.lowerBound(
        sketch.getPositiveValueStore.getDescendingIterator.asScala.next.getIndex
      )
    }
    val minValue = if (sketch.getNegativeValueStore.isEmpty) {
      mapping.lowerBound(
        sketch.getPositiveValueStore.getAscendingIterator.asScala.next.getIndex
      )
    } else {
      -mapping.lowerBound(
        sketch.getNegativeValueStore.getDescendingIterator.asScala.next.getIndex
      )
    }

    value * (1 + Histogram.Tolerance) < minValue || value * (1 - Histogram.Tolerance) > maxValue;
  }
}

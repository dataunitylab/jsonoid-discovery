package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scala.collection.mutable.ListBuffer
import scala.math.{exp, floor, log, random}
import scala.util.Random

import Helpers._

object SamplesProperty {
  val MaxSamples: Int = 100
  val MaxStringLength: Int = 100
  def apply[T](value: T): SamplesProperty[T] = {
    SamplesProperty[T](List(value))
  }
}

final case class SamplesProperty[T](
    val samples: List[T] = List.empty[T],
    val totalSamples: BigInt = 0,
    val nextSample: BigInt = 0,
    val sampleW: Double = 0
) {
  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  def mergeValue(value: T): SamplesProperty[T] = {
    // Only keep the first SamplesProperty.MaxStringLength characters of strings
    val sampleValue = value match {
      case strVal: String =>
        if (strVal.length > SamplesProperty.MaxStringLength) {
          (strVal.take(SamplesProperty.MaxStringLength) + "…").asInstanceOf[T]
        } else {
          strVal
        }
      case _ => value
    }

    // Fill the reservoir with samples
    var newSampleW = sampleW
    var newNextSample = nextSample
    val newSamples = if (samples.length < SamplesProperty.MaxSamples) {
      sampleValue :: samples
    } else if ((totalSamples + 1) <= nextSample) {
      // Use Algorithm L to determine the next sample to take
      newNextSample += floor(log(random) / log(1 - sampleW)).toInt + 1
      newSampleW = exp(log(random) / SamplesProperty.MaxSamples)

      val replaceIndex = floor(random * SamplesProperty.MaxSamples).toInt
      samples.slice(0, replaceIndex) ++ List(sampleValue) ++ samples.drop(
        replaceIndex + 1
      )
    }

    SamplesProperty[T](samples, totalSamples + 1, newNextSample, newSampleW)
  }

  @SuppressWarnings(
    Array(
      "org.wartremover.warts.MutableDataStructures",
      "org.wartremover.warts.Var",
      "org.wartremover.warts.While"
    )
  )
  def merge(other: SamplesProperty[T]): SamplesProperty[T] = {
    // Track already samples values from each set
    val aIndexes = ListBuffer(Random.shuffle(1 to samples.length): _*)
    val bIndexes = ListBuffer(Random.shuffle(1 to other.samples.length): _*)

    val sampleRatio = totalSamples.toDouble * 1.0 /
      (totalSamples.toDouble + other.totalSamples.toDouble)
    var newSamples: ListBuffer[T] = ListBuffer.empty[T]

    // Randomly sample elements proportional to their original frequency
    while (
      newSamples.length < SamplesProperty.MaxSamples && (aIndexes.length > 0 || bIndexes.length > 0)
    ) {
      if (
        aIndexes.length > 0 && (bIndexes.length === 0 || random <= sampleRatio)
      ) {
        newSamples.append(samples(aIndexes.remove(0) - 1))
      } else {
        newSamples.append(other.samples(bIndexes.remove(0) - 1))
      }
    }

    SamplesProperty(
      newSamples.toList,
      totalSamples + other.totalSamples,
      nextSample + other.nextSample,
      0
    )
  }
}

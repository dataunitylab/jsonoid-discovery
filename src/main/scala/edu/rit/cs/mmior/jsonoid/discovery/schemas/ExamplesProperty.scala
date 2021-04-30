package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scala.collection.mutable.ListBuffer
import scala.math.{exp, floor, log, random}
import scala.util.Random

import Helpers._

object ExamplesProperty {
  val MaxExamples: Int = 100

  val MaxStringLength: Int = 100

  def apply[T](value: T): ExamplesProperty[T] = {
    ExamplesProperty[T](List(sampleValue(value)), 1)
  }

  def sampleValue[T](value: T): T = value match {
    case strVal: String =>
      if (strVal.length > ExamplesProperty.MaxStringLength) {
        (strVal.take(ExamplesProperty.MaxStringLength) + "…").asInstanceOf[T]
      } else {
        strVal.asInstanceOf[T]
      }
    case _ => value
  }
}

final case class ExamplesProperty[T](
    val examples: List[T] = List.empty[T],
    val totalExamples: BigInt = 0,
    val nextSample: BigInt = 0,
    val sampleW: Double = 0
) {
  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  def mergeValue(value: T): ExamplesProperty[T] = {
    // Only keep the first ExamplesProperty.MaxStringLength characters of strings
    val sampleValue = ExamplesProperty.sampleValue(value)

    // Fill the reservoir with examples
    var newSampleW = sampleW
    var newNextSample = nextSample
    val newExamples = if (examples.length < ExamplesProperty.MaxExamples) {
      sampleValue :: examples
    } else if ((totalExamples + 1) <= nextSample) {
      // Use Algorithm L to determine the next sample to take
      newNextSample += floor(log(random) / log(1 - sampleW)).toInt + 1
      newSampleW = exp(log(random) / ExamplesProperty.MaxExamples)

      val replaceIndex = floor(random * ExamplesProperty.MaxExamples).toInt
      examples.slice(0, replaceIndex) ++ List(sampleValue) ++ examples.drop(
        replaceIndex + 1
      )
    }

    ExamplesProperty[T](examples, totalExamples + 1, newNextSample, newSampleW)
  }

  @SuppressWarnings(
    Array(
      "org.wartremover.warts.MutableDataStructures",
      "org.wartremover.warts.Var",
      "org.wartremover.warts.While"
    )
  )
  def merge(other: ExamplesProperty[T]): ExamplesProperty[T] = {
    // Track already examples values from each set
    val aIndexes = ListBuffer(Random.shuffle(1 to examples.length): _*)
    val bIndexes = ListBuffer(Random.shuffle(1 to other.examples.length): _*)

    val sampleRatio = totalExamples.toDouble * 1.0 /
      (totalExamples.toDouble + other.totalExamples.toDouble)
    var newExamples: ListBuffer[T] = ListBuffer.empty[T]

    // Randomly sample elements proportional to their original frequency
    while (
      newExamples.length < ExamplesProperty.MaxExamples && (aIndexes.length > 0 || bIndexes.length > 0)
    ) {
      if (
        aIndexes.length > 0 && (bIndexes.length === 0 || random <= sampleRatio)
      ) {
        newExamples.append(examples(aIndexes.remove(0) - 1))
      } else {
        newExamples.append(other.examples(bIndexes.remove(0) - 1))
      }
    }

    ExamplesProperty(
      newExamples.toList,
      totalExamples + other.totalExamples,
      nextSample + other.nextSample,
      0
    )
  }
}

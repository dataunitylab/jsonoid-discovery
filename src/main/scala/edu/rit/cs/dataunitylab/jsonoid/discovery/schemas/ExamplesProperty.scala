package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

import scala.collection.mutable.ListBuffer
import scala.math.{exp, floor, log}
import scala.util.Random

import Helpers._

object ExamplesProperty {
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

/** Used to track examples across atomic values collected from schemas.
  *
  * @constructor Create a new set of examples
  * @param examples a list of example values
  * @param totalExamples the total number of examples seen so far
  * @param nextSample the bounds on the next sample to consider
  * @param sampleW weight to determine the next sample
  */
final case class ExamplesProperty[T](
    val examples: List[T] = List.empty[T],
    val totalExamples: BigInt = 0,
    val nextSample: BigInt = 0,
    val sampleW: Double = 0
) {

  /** Add a new example to the set of examples.
    *
    * @param value the new example to merge
    *
    * @return a new set of examples containing the new example
    */
  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  def mergeValue(value: T)(implicit p: JsonoidParams): ExamplesProperty[T] = {
    // Only keep the first ExamplesProperty.MaxStringLength characters of strings
    val sampleValue = ExamplesProperty.sampleValue(value)

    // Fill the reservoir with examples
    var newSampleW = sampleW
    var newNextSample = nextSample
    val newExamples = if (examples.contains(sampleValue)) {
      examples
    } else if (examples.length < p.maxExamples) {
      sampleValue :: examples
    } else if ((totalExamples + 1) <= nextSample) {
      // Use Algorithm L to determine the next sample to take
      newNextSample += floor(
        log(Random.nextFloat()) / log(1 - sampleW)
      ).toInt + 1
      newSampleW = exp(log(Random.nextFloat()) / p.maxExamples)

      val replaceIndex = floor(Random.nextFloat() * p.maxExamples).toInt
      examples.slice(0, replaceIndex) ++ List(sampleValue) ++ examples.drop(
        replaceIndex + 1
      )
    } else {
      examples
    }

    // We must not have fewer examples
    assert(newExamples.length >= examples.length)

    ExamplesProperty[T](
      newExamples,
      totalExamples + 1,
      newNextSample,
      newSampleW
    )
  }

  /** Combine two sets of examples by merging according to each weight.
    *
    * @param other the other set of examples to merge
    *
    * @return a merged set of examples
    */
  @SuppressWarnings(
    Array(
      "org.wartremover.warts.MutableDataStructures",
      "org.wartremover.warts.NonUnitStatements",
      "org.wartremover.warts.Var",
      "org.wartremover.warts.While"
    )
  )
  def merge(
      other: ExamplesProperty[T]
  )(implicit p: JsonoidParams): ExamplesProperty[T] = {
    // Track already examples values from each set
    val aIndexes = ListBuffer(Random.shuffle(1 to examples.length): _*)
    val bIndexes = ListBuffer(Random.shuffle(1 to other.examples.length): _*)

    val sampleRatio = totalExamples.toDouble * 1.0 /
      (totalExamples.toDouble + other.totalExamples.toDouble)
    val newExamples: ListBuffer[T] = ListBuffer.empty[T]

    // Randomly sample elements proportional to their original frequency
    while (
      newExamples.length < p.maxExamples && (aIndexes.nonEmpty || bIndexes.nonEmpty)
    ) {
      if (
        aIndexes.nonEmpty && (bIndexes.length === 0 || Random
          .nextFloat() <= sampleRatio)
      ) {
        newExamples.append(examples(aIndexes.remove(0) - 1))
      } else {
        newExamples.append(other.examples(bIndexes.remove(0) - 1))
      }
    }

    // We cannot have more examples than both combined
    assert(newExamples.length <= (examples.length + other.examples.length))

    ExamplesProperty(
      newExamples.toList.distinct,
      totalExamples + other.totalExamples,
      nextSample + other.nextSample,
      0
    )
  }
}

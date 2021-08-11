package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import java.net.URI
import java.time.{LocalDate, OffsetDateTime, OffsetTime}
import java.util.UUID
import scala.util.matching.Regex
import scala.util.Try

import com.sangupta.bloomfilter.impl.RoaringBloomFilter
import scalaz._
import org.json4s.JsonDSL._
import org.json4s._
import Scalaz._

import Helpers._
import utils.HyperLogLog

object StringSchema {
  def apply(value: String): StringSchema = {
    StringSchema(StringSchema.AllProperties.mergeValue(value))
  }

  val AllProperties: SchemaProperties[String] = {
    val props = SchemaProperties.empty[String]
    props.add(MinLengthProperty())
    props.add(MaxLengthProperty())
    props.add(StringHyperLogLogProperty())
    props.add(StringBloomFilterProperty())
    props.add(StringExamplesProperty())
    props.add(FormatProperty())
    props.add(PatternProperty())

    props
  }
}

final case class StringSchema(
    override val properties: SchemaProperties[String] =
      StringSchema.AllProperties
) extends JsonSchema[String] {
  override val schemaType = "string"

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ StringSchema(otherProperties) =>
      StringSchema(properties.merge(otherProperties))
  }

  override def copy(properties: SchemaProperties[String]): StringSchema =
    StringSchema(properties)
}

final case class MinLengthProperty(minLength: Option[Int] = None)
    extends SchemaProperty[String, MinLengthProperty] {
  override def toJson: JObject = ("minLength" -> minLength)

  override def merge(otherProp: MinLengthProperty): MinLengthProperty = {
    MinLengthProperty(minOrNone(minLength, otherProp.minLength))
  }

  override def mergeValue(value: String): MinLengthProperty = {
    MinLengthProperty(minOrNone(Some(value.length), minLength))
  }
}

final case class MaxLengthProperty(maxLength: Option[Int] = None)
    extends SchemaProperty[String, MaxLengthProperty] {
  override def toJson: JObject = ("maxLength" -> maxLength)

  override def merge(otherProp: MaxLengthProperty): MaxLengthProperty = {
    MaxLengthProperty(maxOrNone(maxLength, otherProp.maxLength))
  }

  override def mergeValue(value: String): MaxLengthProperty = {
    MaxLengthProperty(maxOrNone(Some(value.length), maxLength))
  }
}

final case class StringHyperLogLogProperty(
    hll: HyperLogLog = new HyperLogLog()
) extends SchemaProperty[String, StringHyperLogLogProperty] {
  override def toJson: JObject = ("distinctValues" -> hll.count())

  override def merge(
      otherProp: StringHyperLogLogProperty
  ): StringHyperLogLogProperty = {
    val prop = StringHyperLogLogProperty()
    prop.hll.merge(this.hll)
    prop.hll.merge(otherProp.hll)

    prop
  }

  override def mergeValue(value: String): StringHyperLogLogProperty = {
    val prop = StringHyperLogLogProperty()
    prop.hll.merge(this.hll)
    prop.hll.addString(value)

    prop
  }
}

object StringBloomFilterProperty {
  val ExpectedElements: Int = 100000
  val FalsePositive: Double = 0.01
}

final case class StringBloomFilterProperty(
    bloomFilter: RoaringBloomFilter[String] = new RoaringBloomFilter[String](
      StringBloomFilterProperty.ExpectedElements,
      StringBloomFilterProperty.FalsePositive
    )
) extends SchemaProperty[String, StringBloomFilterProperty] {
  override def toJson: JObject = JObject(Nil)

  override def merge(
      otherProp: StringBloomFilterProperty
  ): StringBloomFilterProperty = {
    val prop = StringBloomFilterProperty()
    prop.bloomFilter.merge(this.bloomFilter)
    prop.bloomFilter.merge(otherProp.bloomFilter)

    prop
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def mergeValue(value: String): StringBloomFilterProperty = {
    val prop = StringBloomFilterProperty()
    prop.bloomFilter.merge(this.bloomFilter)
    prop.bloomFilter.add(value)

    prop
  }
}

final case class StringExamplesProperty(
    examples: ExamplesProperty[String] = ExamplesProperty()
) extends SchemaProperty[String, StringExamplesProperty] {
  override def toJson: JObject = ("examples" ->
    examples.examples.distinct.sorted)

  override def merge(
      otherProp: StringExamplesProperty
  ): StringExamplesProperty = {
    StringExamplesProperty(examples.merge(otherProp.examples))
  }

  override def mergeValue(value: String): StringExamplesProperty = {
    StringExamplesProperty(examples.merge(ExamplesProperty(value)))
  }
}

object FormatProperty {
  def regex(expr: Regex): Function1[String, Boolean] = { str =>
    !expr.anchored.findFirstIn(str.trim).isEmpty
  }

  val FormatCheckers: Map[String, Function1[String, Boolean]] = Map(
    ("date", str => Try { LocalDate.parse(str) }.isSuccess),
    ("date-time", str => Try { OffsetDateTime.parse(str) }.isSuccess),
    ("time", str => Try { OffsetTime.parse(str) }.isSuccess),
    ("uri", str => Try { new URI(str).getScheme().length > 0 }.isSuccess),
    ("uuid", str => Try { UUID.fromString(str) }.isSuccess),
    ("email", regex("(?=[^\\s]+)(?=(\\w+)@([\\w\\.]+))".r)),
    (
      "ipv4",
      regex(
        "(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)".r
      )
    ),
    (
      "ipv6",
      regex(
        "^(?:(?:(?:[a-fA-F0-9]{1,4}:){6}|(?=(?:[A-F0-9]{0,4}:){0,6}(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$)(([0-9a-fA-F]{1,4}:){0,5}|:)((:[0-9a-fA-F]{1,4}){1,5}:|:))(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)|(?:[a-fA-F0-9]{1,4}:){7}[a-fA-F0-9]{1,4}|(?=(?:[a-fA-F0-9]{0,4}:){0,7}[a-fA-F0-9]{0,4}$)(([0-9a-fA-F]{1,4}:){1,7}|:)((:[0-9a-fA-F]{1,4}){1,7}|:))$".r
      )
    )
  )
}

final case class FormatProperty(
    formats: Map[String, BigInt] = Map.empty[String, BigInt]
) extends SchemaProperty[String, FormatProperty] {
  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  override def toJson: JObject = if (formats.isEmpty) {
    Nil
  } else {
    ("format" -> formats.maxBy(_._2)._1)
  }

  override def merge(
      otherProp: FormatProperty
  ): FormatProperty = {
    val merged = formats.toSeq ++ otherProp.formats.toSeq
    val grouped = merged.groupBy(_._1)
    FormatProperty(grouped.mapValues(_.map(_._2).sum).map(identity).toMap)
  }

  override def mergeValue(value: String): FormatProperty = {
    FormatProperty.FormatCheckers.toSeq.find { case (format, fn) =>
      fn(value)
    } match {
      case Some(format) => merge(FormatProperty(Map((format._1, 1))))
      case None         => this
    }
  }
}

object PatternProperty {
  val ReplaceRegex: Regex =
    ("[" + ".^$()|[]+*?{}".replaceAll(".", "\\\\$0") + "]").r
  val MinExamples: Int = 10
}

final case class PatternProperty(
    prefix: Option[String] = None,
    suffix: Option[String] = None,
    examples: Int = 0,
    minLength: Option[Int] = None
) extends SchemaProperty[String, PatternProperty] {
  override def toJson: JObject = if (examples >= PatternProperty.MinExamples) {
    (prefix.getOrElse(""), suffix.getOrElse("")) match {
      case (str1, str2)
          if str1.length > 0 && str2.length > 0 &&
            // The length checks below ensures we don't end up with
            // prefixes and suffixes which overlap since these can't
            // be converted into a meaningful regex with this approach
            (str1.length + str2.length) < minLength.getOrElse(0) =>
        ("pattern" ->
          ("^" +
            PatternProperty.ReplaceRegex.replaceAllIn(str1, "\\\\$0") +
            ".*" +
            PatternProperty.ReplaceRegex.replaceAllIn(str2, "\\\\$0") +
            "$"))
      case (str, "") if str.length > 0 =>
        ("pattern" -> ("^" +
          PatternProperty.ReplaceRegex.replaceAllIn(str, "\\\\$0")))
      case ("", str) if str.length > 0 =>
        ("pattern" ->
          (PatternProperty.ReplaceRegex.replaceAllIn(str, "\\\\$0") + "$"))
      case (_, _) => Nil
    }
  } else {
    Nil
  }

  private def findCommonPrefix(
      str1: Option[String],
      str2: Option[String]
  ): Option[String] = {
    (str1, str2) match {
      case (Some(str1), Some(str2)) =>
        Some(
          (str1, str2).zipped
            .takeWhile(Function.tupled(_ == _))
            .map(_._1)
            .mkString
        )
      case (None, x) => x
      case (x, None) => x
    }
  }

  override def merge(
      otherProp: PatternProperty
  ): PatternProperty = {
    val newPrefix = findCommonPrefix(prefix, otherProp.prefix)
    val newSuffix =
      findCommonPrefix(
        suffix.map(_.reverse),
        otherProp.suffix.map(_.reverse)
      ).map(_.reverse)
    PatternProperty(
      newPrefix,
      newSuffix,
      examples + otherProp.examples,
      minOrNone(minLength, otherProp.minLength)
    )
  }

  override def mergeValue(value: String): PatternProperty = {
    merge(PatternProperty(Some(value), Some(value), 1, Some(value.length)))
  }
}

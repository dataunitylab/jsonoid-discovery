package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scala.reflect._

import java.net.URI
import java.time.{LocalDate, OffsetDateTime, OffsetTime}
import java.util.UUID
import scala.util.matching.Regex
import scala.util.Try

import scalaz._
import org.apache.commons.validator.routines.EmailValidator
import org.json4s.JsonDSL._
import org.json4s._
import Scalaz._

import Helpers._
import utils.{BloomFilter, Histogram, HyperLogLog}

object StringSchema {
  def apply(
      value: String
  )(implicit propSet: PropertySet, p: JsonoidParams): StringSchema = {
    StringSchema(
      propSet.stringProperties.mergeValue(value)(p)
    )
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
    props.add(StringLengthHistogramProperty())

    props
  }

  val MinProperties: SchemaProperties[String] = {
    SchemaProperties.empty[String]
  }

  val SimpleProperties: SchemaProperties[String] = {
    val props = SchemaProperties.empty[String]
    props.add(MinLengthProperty())
    props.add(MaxLengthProperty())
    props.add(FormatProperty())
    props.add(PatternProperty())
    props.add(StringExamplesProperty())

    props
  }
}

final case class StringSchema(
    override val properties: SchemaProperties[String] =
      StringSchema.AllProperties
) extends JsonSchema[String] {
  override val schemaType = "string"

  override val validTypes: Set[Class[_]] = Set(classOf[JString])

  override def mergeSameType(mergeType: MergeType)(implicit
      p: JsonoidParams
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ StringSchema(otherProperties) =>
      StringSchema(properties.merge(otherProperties, mergeType))
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def copy(properties: SchemaProperties[String]): StringSchema = {
    val newSchema = StringSchema(properties)
    newSchema.definitions ++= this.definitions

    newSchema
  }
}

final case class MinLengthProperty(minLength: Option[Int] = None)
    extends SchemaProperty[String, MinLengthProperty] {
  override def toJson()(implicit p: JsonoidParams): JObject =
    ("minLength" -> minLength)

  override def intersectMerge(
      otherProp: MinLengthProperty
  )(implicit p: JsonoidParams): MinLengthProperty = {
    MinLengthProperty(maxOrNone(minLength, otherProp.minLength))
  }

  override def unionMerge(
      otherProp: MinLengthProperty
  )(implicit p: JsonoidParams): MinLengthProperty = {
    MinLengthProperty(minOrNone(minLength, otherProp.minLength))
  }

  override def mergeValue(
      value: String
  )(implicit p: JsonoidParams): MinLengthProperty = {
    MinLengthProperty(minOrNone(Some(value.length), minLength))
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      tag: ClassTag[S]
  ) = {
    value match {
      case JString(str) =>
        minLength match {
          case Some(length) =>
            if (str.length < length) {
              Seq(
                Anomaly(path, "string shorter than minimum length", Warning)
              )
            } else {
              Seq.empty
            }
          case None => Seq.empty
        }
      case _ => Seq.empty
    }
  }

  override def isCompatibleWith(
      other: MinLengthProperty
  )(implicit p: JsonoidParams): Boolean = {
    Helpers.isMinCompatibleWith(minLength, false, other.minLength, false)
  }
}

final case class MaxLengthProperty(maxLength: Option[Int] = None)
    extends SchemaProperty[String, MaxLengthProperty] {
  override def toJson()(implicit p: JsonoidParams): JObject =
    ("maxLength" -> maxLength)

  override def intersectMerge(
      otherProp: MaxLengthProperty
  )(implicit p: JsonoidParams): MaxLengthProperty = {
    MaxLengthProperty(minOrNone(maxLength, otherProp.maxLength))
  }

  override def unionMerge(
      otherProp: MaxLengthProperty
  )(implicit p: JsonoidParams): MaxLengthProperty = {
    MaxLengthProperty(maxOrNone(maxLength, otherProp.maxLength))
  }

  override def mergeValue(
      value: String
  )(implicit p: JsonoidParams): MaxLengthProperty = {
    MaxLengthProperty(maxOrNone(Some(value.length), maxLength))
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      tag: ClassTag[S]
  ) = {
    value match {
      case JString(str) =>
        maxLength match {
          case Some(length) =>
            if (str.length > length) {
              Seq(
                Anomaly(path, "string longer than maximum length", Warning)
              )
            } else {
              Seq.empty
            }
          case None => Seq.empty
        }
      case _ => Seq.empty
    }
  }
  override def isCompatibleWith(
      other: MaxLengthProperty
  )(implicit p: JsonoidParams): Boolean = {
    Helpers.isMaxCompatibleWith(maxLength, false, other.maxLength, false)
  }
}

final case class StringHyperLogLogProperty(
    hll: HyperLogLog = new HyperLogLog()
) extends SchemaProperty[String, StringHyperLogLogProperty] {
  override val isInformational = true

  override def toJson()(implicit p: JsonoidParams): JObject =
    ("distinctValues" -> hll.count()) ~ ("hll" ->
      hll.toBase64)

  override def unionMerge(
      otherProp: StringHyperLogLogProperty
  )(implicit p: JsonoidParams): StringHyperLogLogProperty = {
    val prop = StringHyperLogLogProperty()
    prop.hll.merge(this.hll)
    prop.hll.merge(otherProp.hll)

    prop
  }

  override def mergeValue(
      value: String
  )(implicit p: JsonoidParams): StringHyperLogLogProperty = {
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
    bloomFilter: BloomFilter[String] = BloomFilter[String]()
) extends SchemaProperty[String, StringBloomFilterProperty] {
  override val isInformational = true

  override def toJson()(implicit p: JsonoidParams): JObject =
    ("bloomFilter" -> bloomFilter.toBase64)

  override def unionMerge(
      otherProp: StringBloomFilterProperty
  )(implicit p: JsonoidParams): StringBloomFilterProperty = {
    val prop = StringBloomFilterProperty()
    prop.bloomFilter.filter.merge(this.bloomFilter.filter)
    prop.bloomFilter.filter.merge(otherProp.bloomFilter.filter)

    prop
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def mergeValue(
      value: String
  )(implicit p: JsonoidParams): StringBloomFilterProperty = {
    val prop = StringBloomFilterProperty()
    prop.bloomFilter.filter.merge(this.bloomFilter.filter)
    prop.bloomFilter.filter.add(value)

    prop
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      tag: ClassTag[S]
  ) = {
    val inFilter = value match {
      case JString(str) => Some(bloomFilter.contains(str))
      case _            => None
    }

    inFilter match {
      case Some(false) =>
        Seq(Anomaly(path, "value not found in Bloom filter", Info))
      case _ => Seq.empty
    }
  }
}

final case class StringExamplesProperty(
    examples: ExamplesProperty[String] = ExamplesProperty()
) extends SchemaProperty[String, StringExamplesProperty] {
  override val isInformational = true

  override def toJson()(implicit p: JsonoidParams): JObject = ("examples" ->
    examples.examples.distinct.sorted)

  override def unionMerge(
      otherProp: StringExamplesProperty
  )(implicit p: JsonoidParams): StringExamplesProperty = {
    StringExamplesProperty(examples.merge(otherProp.examples))
  }

  override def mergeValue(
      value: String
  )(implicit p: JsonoidParams): StringExamplesProperty = {
    StringExamplesProperty(examples.merge(ExamplesProperty(value)))
  }
}

object FormatProperty {
  val MinExamples: Int = 10

  def regex(expr: Regex): Function1[String, Boolean] = { str =>
    expr.anchored.findFirstIn(str.trim).isDefined
  }

  val FormatCheckers: Map[String, Function1[String, Boolean]] = Map(
    ("date", str => Try { LocalDate.parse(str) }.isSuccess),
    ("date-time", str => Try { OffsetDateTime.parse(str) }.isSuccess),
    ("time", str => Try { OffsetTime.parse(str) }.isSuccess),
    (
      "uri",
      str => Try { new URI(str).getScheme().length > 0 }.getOrElse(false)
    ),
    ("uuid", str => Try { UUID.fromString(str) }.isSuccess),
    (
      "email",
      str => Try { EmailValidator.getInstance().isValid(str) }.getOrElse(false)
    ),
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
  @SuppressWarnings(
    Array(
      "org.wartremover.warts.Equals",
      "org.wartremover.warts.TraversableOps"
    )
  )
  private def maxFormat()(implicit p: JsonoidParams) = {
    val total = formats.values.sum
    if (total >= FormatProperty.MinExamples) {
      val maxFormat = formats.maxBy(_._2)
      if (
        BigDecimal(maxFormat._2.toDouble) / BigDecimal(
          total
        ) >= p.formatThreshold && maxFormat._1 != "none"
      ) {
        Some(maxFormat._1)
      } else {
        None
      }
    } else {
      None
    }
  }

  override def toJson()(implicit p: JsonoidParams): JObject = {
    maxFormat match {
      case Some(format) => ("format" -> format)
      case None         => Nil
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  override def intersectMerge(
      otherProp: FormatProperty
  )(implicit p: JsonoidParams): FormatProperty = {
    val merged = formats.toSeq ++ otherProp.formats.toSeq
    val grouped = merged.groupBy(_._1)
    FormatProperty(
      grouped.mapValues(_.map(_._2).min).filter(_._2 > 0).map(identity).toMap
    )
  }

  override def unionMerge(
      otherProp: FormatProperty
  )(implicit p: JsonoidParams): FormatProperty = {
    val merged = formats.toSeq ++ otherProp.formats.toSeq
    val grouped = merged.groupBy(_._1)
    FormatProperty(grouped.mapValues(_.map(_._2).sum).map(identity).toMap)
  }

  override def mergeValue(
      value: String
  )(implicit p: JsonoidParams): FormatProperty = {
    FormatProperty.FormatCheckers.toSeq.find { case (format, fn) =>
      fn(value)
    } match {
      case Some(format) => unionMerge(FormatProperty(Map((format._1, 1))))
      case None         => unionMerge(FormatProperty(Map(("none", 1))))
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def isCompatibleWith(
      other: FormatProperty
  )(implicit p: JsonoidParams): Boolean = {
    maxFormat.isEmpty || maxFormat == other.maxFormat
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
  override def toJson()(implicit p: JsonoidParams): JObject = if (
    examples >= PatternProperty.MinExamples
  ) {
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

  override def unionMerge(
      otherProp: PatternProperty
  )(implicit p: JsonoidParams): PatternProperty = {
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

  override def mergeValue(
      value: String
  )(implicit p: JsonoidParams): PatternProperty = {
    unionMerge(PatternProperty(Some(value), Some(value), 1, Some(value.length)))
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      tag: ClassTag[S]
  ) = {
    value match {
      case JString(str) =>
        val prefixMatch = str.startsWith(prefix.getOrElse(""))
        val suffixMatch = str.endsWith(suffix.getOrElse(""))

        (if (prefixMatch) {
           Seq.empty
         } else {
           Seq(
             Anomaly(
               path,
               "value does not have the required prefix",
               Fatal
             )
           )
         }) ++ (if (suffixMatch) {
                  Seq.empty
                } else {
                  Seq(
                    Anomaly(
                      path,
                      "value does not have the required suffix",
                      Fatal
                    )
                  )
                })
      case _ => Seq.empty
    }
  }

  override def isCompatibleWith(
      other: PatternProperty
  )(implicit p: JsonoidParams): Boolean = {
    prefix.getOrElse("").startsWith(other.prefix.getOrElse("")) &&
    suffix.getOrElse("").endsWith(other.suffix.getOrElse(""))
  }
}

final case class StaticPatternProperty(regex: Regex)
    extends SchemaProperty[String, StaticPatternProperty] {
  override def mergeable: Boolean = false

  override def toJson()(implicit p: JsonoidParams): JObject =
    ("pattern" -> regex.toString)

  override def unionMerge(
      otherProp: StaticPatternProperty
  )(implicit p: JsonoidParams): StaticPatternProperty = {
    throw new UnsupportedOperationException(
      "StaticPatternProperty cannot be merged"
    )
  }

  override def mergeValue(
      value: String
  )(implicit p: JsonoidParams): StaticPatternProperty = {
    throw new UnsupportedOperationException(
      "StaticPatternProperty cannot be merged"
    )
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      tag: ClassTag[S]
  ) = {
    value match {
      case JString(str) =>
        if (regex.anchored.findFirstIn(str.trim).isEmpty) {
          Seq(Anomaly(path, "value does not match the required regex", Fatal))
        } else {
          Seq.empty
        }
      case _ => Seq.empty
    }
  }

  override def isCompatibleWith(
      other: StaticPatternProperty
  )(implicit p: JsonoidParams): Boolean = {
    // Regexes do not necessarily need to be equal to be
    // compatible, but this is the best that we attempt to do for now
    regex === other.regex
  }
}

final case class StringLengthHistogramProperty(
    histogram: Histogram = Histogram()
) extends SchemaProperty[String, StringLengthHistogramProperty] {
  override val isInformational = true

  override def toJson()(implicit p: JsonoidParams): JObject = {
    ("lengthHistogram" -> histogram.bins.map { case (value, count) =>
      List(value.doubleValue, count.longValue)
    })
  }

  override def unionMerge(
      otherProp: StringLengthHistogramProperty
  )(implicit p: JsonoidParams): StringLengthHistogramProperty = {
    StringLengthHistogramProperty(histogram.merge(otherProp.histogram))
  }

  override def mergeValue(
      value: String
  )(implicit p: JsonoidParams): StringLengthHistogramProperty = {
    StringLengthHistogramProperty(
      histogram.merge(value.length)
    )
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      tag: ClassTag[S]
  ) = {
    value match {
      case JString(str) =>
        if (histogram.isAnomalous(str.length)) {
          Seq(
            Anomaly(path, "string length outside histogram range", Warning)
          )
        } else {
          Seq.empty
        }
      case _ => Seq.empty
    }
  }
}

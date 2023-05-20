package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

import scala.reflect._

import java.net.URI
import java.time.{LocalDate, OffsetDateTime, OffsetTime}
import java.util.UUID
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

import com.google.openlocationcode.OpenLocationCode
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
  )(implicit p: JsonoidParams): StringSchema = {
    StringSchema(
      p.propSet.stringProperties.mergeValue(value)(p)
    )
  }

  /** Convert a serialized JSON value to a string schema object. */
  def fromJson(str: JObject): StringSchema = {
    implicit val formats: Formats = DefaultFormats
    val props = SchemaProperties.empty[String]

    // Create a format property with the given format
    if ((str \ "format") =/= JNothing) {
      try {
        val format = (str \ "format").extract[String]
        props.add(FormatProperty(Map(format -> 1)))
      } catch {
        case e: org.json4s.MappingException =>
      }
    }

    // Use a StaticPatternProperty to represent the pattern
    if ((str \ "pattern") =/= JNothing) {
      try {
        val regexStr = (str \ "pattern").extract[String]
        if (!regexStr.isEmpty) {
          props.add(StaticPatternProperty(regexStr.r))
        }
      } catch {
        case e: java.util.regex.PatternSyntaxException =>
        case e: org.json4s.MappingException            =>
      }
    }

    // Handle minimum and maximum string length
    if ((str \ "minLength") =/= JNothing) {
      try {
        props.add(MinLengthProperty(Some((str \ "minLength").extract[Int])))
      } catch {
        case e: org.json4s.MappingException =>
      }
    }
    if ((str \ "maxLength") =/= JNothing) {
      try {
        props.add(MaxLengthProperty(Some((str \ "maxLength").extract[Int])))
      } catch {
        case e: org.json4s.MappingException =>
      }
    }

    // Add string examples
    if (str.values.contains("examples")) {
      try {
        val examples = (str \ "examples").extract[List[String]]
        props.add(
          StringExamplesProperty(ExamplesProperty(examples, examples.length))
        )
      } catch {
        case e: org.json4s.MappingException =>
      }
    }

    // Deserialize the provided Bloom filter
    if (str.values.contains("bloomFilter")) {
      try {
        val bloomStr = (str \ "bloomFilter").extract[String]
        val bloomFilter = BloomFilter.deserialize[String](bloomStr)
        props.add(StringBloomFilterProperty(bloomFilter))
      } catch {
        case e: org.json4s.MappingException        =>
        case e: java.io.EOFException               =>
        case e: java.io.StreamCorruptedException   =>
        case e: java.lang.IllegalArgumentException =>
      }
    }

    StringSchema(props)
  }

  lazy val AllProperties: SchemaProperties[String] = {
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

  lazy val MinProperties: SchemaProperties[String] = {
    SchemaProperties.empty[String]
  }

  lazy val SimpleProperties: SchemaProperties[String] = {
    val props = SchemaProperties.empty[String]
    props.add(MinLengthProperty())
    props.add(MaxLengthProperty())
    props.add(FormatProperty())
    props.add(PatternProperty())
    props.add(StringExamplesProperty())

    props
  }
}

/** Represents strings in JSON Schema.
  */
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

  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  override def toJson()(implicit p: JsonoidParams): JObject = {
    // If all strings are numbers and we have the numeric property,
    // then use the numeric schema instead of the string schema.
    val maybeNumericProp = properties.getOrNone[StringNumericProperty]
    maybeNumericProp match {
      case Some(numericProp) =>
        if (!numericProp.failed && numericProp.numericSchema.isDefined)
          numericProp.numericSchema.get.toJson()(p)
        else
          super.toJson()(p)
      case None => super.toJson()(p)
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def copy(properties: SchemaProperties[String]): StringSchema = {
    val newSchema = StringSchema(properties)
    newSchema.definitions ++= this.definitions

    newSchema
  }
}

/** Tracks the minimum length of strings.
  *
  * @constructor Create a new minimum string length property
  * @param minLength the minimum string length
  */
final case class MinLengthProperty(minLength: Option[Int] = None)
    extends SchemaProperty[String] {
  override type S = MinLengthProperty

  override def newDefault()(implicit p: JsonoidParams): MinLengthProperty =
    MinLengthProperty()

  override def toJson()(implicit p: JsonoidParams): JObject = {
    // Minimum length must be non-negative
    assert(minLength.getOrElse(0) >= 0)

    ("minLength" -> minLength)
  }

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
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    value match {
      case JString(str) =>
        minLength match {
          case Some(length) =>
            if (str.length < length) {
              Seq(
                Anomaly(
                  path,
                  "string shorter than minimum length",
                  AnomalyLevel.Warning
                )
              )
            } else {
              Seq.empty
            }
          case None => Seq.empty
        }
      case _ => Seq.empty
    }
  }

  override def isSubsetOf(
      other: MinLengthProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    Helpers.isMinCoveredBy(minLength, false, other.minLength, false)
  }

  override def expandTo(other: Option[MinLengthProperty]): MinLengthProperty = {
    val newMin = maybeContractInt(
      minLength,
      other.map(_.minLength).getOrElse(None),
      false,
      other.isEmpty
    )._1

    // Expanded minimum cannot be larger
    assert(newMin.getOrElse(0) <= minLength.getOrElse(0))

    MinLengthProperty(newMin)
  }
}

/** Tracks the maximum length of strings.
  *
  * @constructor Create a new maximum string length property
  * @param maxLength the maximum string length
  */
final case class MaxLengthProperty(maxLength: Option[Int] = None)
    extends SchemaProperty[String] {
  override type S = MaxLengthProperty

  override def newDefault()(implicit p: JsonoidParams): MaxLengthProperty =
    MaxLengthProperty()

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
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    value match {
      case JString(str) =>
        maxLength match {
          case Some(length) =>
            if (str.length > length) {
              Seq(
                Anomaly(
                  path,
                  "string longer than maximum length",
                  AnomalyLevel.Warning
                )
              )
            } else {
              Seq.empty
            }
          case None => Seq.empty
        }
      case _ => Seq.empty
    }
  }

  override def isSubsetOf(
      other: MaxLengthProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    Helpers.isMaxCoveredBy(maxLength, false, other.maxLength, false)
  }

  override def expandTo(other: Option[MaxLengthProperty]): MaxLengthProperty = {
    val newMax = maybeExpandInt(
      maxLength.map(BigInt(_)),
      other.flatMap(_.maxLength.map(BigInt(_))),
      false,
      other.isEmpty
    )._1.map(_.toInt)

    // Expanded maximum cannot be smaller
    assert(newMax.getOrElse(0) >= maxLength.getOrElse(0))

    MaxLengthProperty(newMax)
  }
}

/** Tracks the estimated cardinality of the set of string values.
  *
  * @constructor Create a new HLL property
  * @param hll the HyperLogLog data structure used to track the cardinality
  */
final case class StringHyperLogLogProperty(hll: HyperLogLog = new HyperLogLog())
    extends SchemaProperty[String] {
  override type S = StringHyperLogLogProperty

  override def newDefault()(implicit
      p: JsonoidParams
  ): StringHyperLogLogProperty =
    StringHyperLogLogProperty()

  override val isInformational = true

  override def toJson()(implicit p: JsonoidParams): JObject =
    ("distinctValues" -> hll.count()) ~ ("hll" -> hll.toBase64)

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

/** Tracks possible strings which can be contained in the set
  *
  * @constructor Create a new string Bloom filter property
  * @param bloomFilter the Bloom filter used to track the set of strings
  */
final case class StringBloomFilterProperty(
    bloomFilter: BloomFilter[String] = BloomFilter[String]()
) extends SchemaProperty[String] {
  override type S = StringBloomFilterProperty

  override def newDefault()(implicit
      p: JsonoidParams
  ): StringBloomFilterProperty =
    StringBloomFilterProperty()

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
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    val inFilter = value match {
      case JString(str) => Some(bloomFilter.contains(str))
      case _            => None
    }

    inFilter match {
      case Some(false) =>
        Seq(Anomaly(path, "value not found in Bloom filter", AnomalyLevel.Info))
      case _ => Seq.empty
    }
  }
}

/** Tracks examples observed for these strings.
  *
  * @constructor Create a new string examples property
  * @param examples the example strings observed
  */
final case class StringExamplesProperty(
    examples: ExamplesProperty[String] = ExamplesProperty()
) extends SchemaProperty[String] {
  override type S = StringExamplesProperty

  override def newDefault()(implicit p: JsonoidParams): StringExamplesProperty =
    StringExamplesProperty()

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

  /** The minimum number of examples of a formatted string that must be
    *  observed.
    */
  val MinExamples: Int = 10

  /** Helper function to define elements in [[FormatCheckers]] that use a regex.
    */
  private def regex(expr: Regex): Function1[String, Boolean] = { str =>
    expr.anchored.findFirstIn(str.trim).isDefined
  }

  /** A map from formats to a function which checks the format.
    */
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

  val ExtendedFormatCheckers: Map[String, Function1[String, Boolean]] = Map(
    // https://www.wikidata.org/wiki/Property:P819
    // "A" also seems to be a valid qualifier, so this was added
    (
      "bibcode",
      regex("\\d{4}[A-Za-z\\.&]{5}[\\w\\.]{4}[AELPQ-Z\\.][\\d\\.]{4}[A-Z]".r)
    ),
    // https://github.com/citation-file-format/citation-file-format/blob/4fa1a91004b3600248bb3f751967e833e7afac63/schema.yaml
    (
      "doi",
      regex("""10\.\d{4,9}(\.\d+)?/[A-Za-z0-9-\._;\(\)\[\]\\\\:/]+""".r)
    ),
    // https://www.oreilly.com/library/view/regular-expressions-cookbook/9781449327453/ch04s13.html
    (
      "isbn",
      regex(
        "(?:ISBN(?:-1[03])?:? )?(?=[0-9X]{10}$|(?=(?:[0-9]+[- ]){3})[- 0-9X]{13}$|97[89][0-9]{10}$|(?=(?:[0-9]+[- ]){4})[- 0-9]{17}$)(?:97[89][- ]?)?[0-9]{1,5}[- ]?[0-9]+[- ]?[0-9]+[- ]?[0-9X]".r
      )
    ),
    ("plus-code", OpenLocationCode.isValidCode),
    (
      "geo-uri",
      regex(
        "(?i)^geo:(-?\\d+(\\.\\d+)?),(-?\\d+(\\.\\d+)?)(,(-?\\d+(\\.\\d+)?))?(;(.*))?$".r
      )
    )
  )
}

/** Tracks the possible formats for string values.
  *
  * @constructor Create a new string format property
  * @param formats a map from formats to a count of observed examples
  */
final case class FormatProperty(
    formats: Map[String, BigInt] = Map.empty[String, BigInt]
) extends SchemaProperty[String] {
  override type S = FormatProperty

  override def newDefault()(implicit p: JsonoidParams): FormatProperty =
    FormatProperty()

  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  def maxFormat(
      useLimit: Boolean = true
  )(implicit p: JsonoidParams): Option[String] = {
    val total = formats.values.sum
    if ((total > 0 && !useLimit) || total >= FormatProperty.MinExamples) {
      val maxFormat = formats.maxBy(_._2)
      if (
        BigDecimal(maxFormat._2.toDouble) / BigDecimal(
          total
        ) >= p.formatThreshold && maxFormat._1 =/= "none"
      ) {
        // Format must be one of those listed in a checker
        assert(
          FormatProperty.FormatCheckers.contains(maxFormat._1) ||
            FormatProperty.ExtendedFormatCheckers.contains(maxFormat._1)
        )

        Some(maxFormat._1)
      } else {
        None
      }
    } else {
      None
    }
  }

  override def toJson()(implicit p: JsonoidParams): JObject = {
    maxFormat() match {
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
      grouped.view
        .mapValues(_.map(_._2).min)
        .filter(_._2 > 0)
        .map(identity)
        .toMap
    )
  }

  override def unionMerge(
      otherProp: FormatProperty
  )(implicit p: JsonoidParams): FormatProperty = {
    val merged = formats.toSeq ++ otherProp.formats.toSeq
    val grouped = merged.groupBy(_._1)
    val newFormat = FormatProperty(
      grouped.view.mapValues(_.map(_._2).sum).map(identity).toMap
    )

    // If we have a format, it must be one of the two
    assert(
      maxFormat().isEmpty || List(maxFormat(), otherProp.maxFormat())
        .contains(newFormat.maxFormat())
    )

    newFormat
  }

  override def mergeValue(
      value: String
  )(implicit p: JsonoidParams): FormatProperty = {
    // Select which checkers to use. If chosen, we start with the
    // extended checkers since some of them are more specific
    // (e.g. "geo-uri" is a more specific format for "uri").
    val checkers =
      (if (p.extendedFormats)
         FormatProperty.ExtendedFormatCheckers.toSeq
       else
         Seq.empty) ++ FormatProperty.FormatCheckers

    checkers.find { case (format, fn) =>
      fn(value)
    } match {
      case Some(format) => unionMerge(FormatProperty(Map((format._1, 1))))
      case None         => unionMerge(FormatProperty(Map(("none", 1))))
    }
  }

  override def isSubsetOf(
      other: FormatProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    // Ignore minimum examples for this schema since we may not have
    // received enough evidence yet to pick the correct format,
    // but these schemas should still be considered compatible
    val otherFormat = other.maxFormat()
    otherFormat.isEmpty || maxFormat(false) === otherFormat
  }

  override def expandTo(other: Option[FormatProperty]): FormatProperty = {
    if (maxFormat() === other.flatMap(_.maxFormat(false)))
      this
    else
      // Reset to empty formats
      FormatProperty()
  }
}

object PatternProperty {
  val ReplaceRegex: Regex =
    ("[" + ".^$()|[]+*?{}".replaceAll(".", "\\\\$0") + "]").r
  val MinExamples: Int = 10
}

/** Tracks the possible patterns for string values.
  *
  * @constructor Create a new string pattern property
  * @param prefix a possible common prefix of strings
  * @param suffix a possible common suffix of strings
  * @param examples the total number of observed examples
  * @param minLength the minimum observed length of a string
  */
final case class PatternProperty(
    prefix: Option[String] = None,
    suffix: Option[String] = None,
    examples: Int = 0,
    minLength: Option[Int] = None
) extends SchemaProperty[String] {
  override type S = PatternProperty

  override def newDefault()(implicit p: JsonoidParams): PatternProperty =
    PatternProperty()

  private def isValidPattern: Boolean = {
    val prefixLength = prefix.map(_.length).getOrElse(0)
    val suffixLength = suffix.map(_.length).getOrElse(0)

    // The length checks below ensures we don't end up with
    // prefixes and suffixes which overlap since these can't
    // be converted into a meaningful regex with this approach
    examples >= PatternProperty.MinExamples &&
    (prefixLength > 0 || suffixLength > 0) &&
    (prefixLength + suffixLength) < minLength.getOrElse(0)
  }

  override def toJson()(implicit p: JsonoidParams): JObject = if (
    isValidPattern
  ) {
    (prefix.getOrElse(""), suffix.getOrElse("")) match {
      case (str1, str2) if str1.length > 0 && str2.length > 0 =>
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

  private def patternAnomalyLevel(value: String): AnomalyLevel = {
    // If the string consists of only digits, treat
    // it as a warning like with numeric types
    if (value.forall(_.isDigit))
      AnomalyLevel.Warning
    else
      AnomalyLevel.Fatal
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    value match {
      case JString(str) if isValidPattern =>
        val prefixMatch = str.startsWith(prefix.getOrElse(""))
        val suffixMatch = str.endsWith(suffix.getOrElse(""))

        (if (prefixMatch) {
           Seq.empty
         } else {
           Seq(
             Anomaly(
               path,
               "value does not have the required prefix",
               patternAnomalyLevel(str)
             )
           )
         }) ++ (if (suffixMatch) {
                  Seq.empty
                } else {
                  Seq(
                    Anomaly(
                      path,
                      "value does not have the required suffix",
                      patternAnomalyLevel(str)
                    )
                  )
                })
      case _ => Seq.empty
    }
  }

  override def isSubsetOf(
      other: PatternProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    if (other.isValidPattern) {
      prefix.getOrElse("").startsWith(other.prefix.getOrElse("")) &&
      suffix.getOrElse("").endsWith(other.suffix.getOrElse(""))
    } else {
      true
    }
  }

  override def expandTo(other: Option[PatternProperty]): PatternProperty = {
    other match {
      case Some(otherProp) =>
        if (otherProp.isSubsetOf(this))
          this
        else
          // TODO Work on heuristics for expansion
          PatternProperty()
      case None => PatternProperty()
    }
  }
}

/** Represents patterns read in from a serialized JSON Schema.
  *
  * @constructor Create a new static string pattern property
  * @param regex a regular expression which represents the pattern to match
  */
final case class StaticPatternProperty(regex: Regex)
    extends SchemaProperty[String] {
  // Regexes must be non-empty
  assert(regex.toString.nonEmpty)

  override type S = StaticPatternProperty

  override def newDefault()(implicit p: JsonoidParams): StaticPatternProperty =
    StaticPatternProperty(".*".r)

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
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    value match {
      case JString(str) =>
        if (regex.findFirstIn(str.trim).isEmpty) {
          Seq(
            Anomaly(
              path,
              "value does not match the required regex",
              AnomalyLevel.Fatal
            )
          )
        } else {
          Seq.empty
        }
      case _ => Seq.empty
    }
  }

  override def isSubsetOf(
      other: StaticPatternProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    // Regexes do not necessarily need to be equal to be
    // compatible, but this is the best that we attempt to do for now
    regex.regex === other.regex.regex
  }
}

/** Tracks a histogram of string lengths.
  *
  * @constructor Create a new string length histogram property
  * @param histogram a histogram of string lengths
  */
final case class StringLengthHistogramProperty(
    histogram: Histogram = Histogram()
) extends SchemaProperty[String] {
  override type S = StringLengthHistogramProperty

  override def newDefault()(implicit
      p: JsonoidParams
  ): StringLengthHistogramProperty =
    StringLengthHistogramProperty()

  override val isInformational = true

  override def toJson()(implicit p: JsonoidParams): JObject = {
    ("lengthHistogram" -> histogram.toJson)
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
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    value match {
      case JString(str) =>
        if (histogram.isAnomalous(str.length)) {
          Seq(
            Anomaly(
              path,
              "string length outside histogram range",
              AnomalyLevel.Info
            )
          )
        } else {
          Seq.empty
        }
      case _ => Seq.empty
    }
  }
}

/** Tracks a possible numeric schema.
  *
  * @constructor Create a new numeric string schema property
  * @param numericSchema a possible numeric schema
  */
final case class StringNumericProperty(
    numericSchema: Option[NumberSchema] = None,
    failed: Boolean = false
) extends SchemaProperty[String] {
  override type S = StringNumericProperty

  override def newDefault()(implicit
      p: JsonoidParams
  ): StringNumericProperty =
    StringNumericProperty()

  override val isInformational = false

  override def toJson()(implicit p: JsonoidParams): JObject = Nil

  override def unionMerge(
      otherProp: StringNumericProperty
  )(implicit p: JsonoidParams): StringNumericProperty = {
    if (failed || otherProp.failed) {
      StringNumericProperty(None, true)
    } else {
      (numericSchema, otherProp.numericSchema) match {
        case (Some(n1), Some(n2)) =>
          StringNumericProperty(Some(n1.merge(n2).asInstanceOf[NumberSchema]))
        case (Some(n1), None) => StringNumericProperty(Some(n1))
        case (None, Some(n2)) => StringNumericProperty(Some(n2))
        case (None, None)     => this
      }
    }
  }

  override def mergeValue(
      value: String
  )(implicit p: JsonoidParams): StringNumericProperty = {
    Try(BigDecimal(value)) match {
      case Success(num) =>
        val newSchema = Some(NumberSchema(num)(p))
        unionMerge(StringNumericProperty(newSchema, false))
      case Failure(_) => StringNumericProperty(None, true)
    }
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    numericSchema match {
      case None => Seq.empty
      case Some(schema) =>
        value match {
          case JString(str) => {
            Try(BigDecimal(str)) match {
              case Success(num) =>
                schema.collectAnomalies(JDecimal(num), path)
              case Failure(_) =>
                Seq(Anomaly(path, "value is not a number", AnomalyLevel.Fatal))
            }
          }
          case _ => Seq.empty
        }
    }
  }
}

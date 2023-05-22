package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

import scala.reflect._

import scalaz._
import org.json4s.JsonDSL._
import org.json4s._
import Scalaz._

import Helpers._
import utils.{BloomFilter, Histogram, HyperLogLog}

object IntegerSchema {
  def apply(
      value: BigInt
  )(implicit p: JsonoidParams): IntegerSchema = {
    IntegerSchema(
      p.propSet.integerProperties.mergeValue(value)(p)
    )
  }

  /** Convert a serialized JSON value to an integer schema object. */
  def fromJson(int: JObject): IntegerSchema = {
    implicit val formats: Formats = DefaultFormats
    val props = SchemaProperties.empty[BigInt]

    if (int.values.contains("multipleOf")) {
      try {
        props.add(
          IntMultipleOfProperty(Some((int \ "multipleOf").extract[BigInt]))
        )
      } catch {
        // $COVERAGE-OFF$
        case e: org.json4s.MappingException =>
        // $COVERAGE-ON$
      }
    }

    if (int.values.contains("minimum")) {
      try {
        props.add(MinIntValueProperty(Some((int \ "minimum").extract[BigInt])))
      } catch {
        // $COVERAGE-OFF$
        case e: org.json4s.MappingException =>
        // $COVERAGE-ON$
      }
    }

    if (int.values.contains("exclusiveMinimum")) {
      try {
        props.add(
          MinIntValueProperty(
            Some((int \ "exclusiveMinimum").extract[BigInt]),
            true
          )
        )
      } catch {
        // $COVERAGE-OFF$
        case e: org.json4s.MappingException =>
        // $COVERAGE-ON$
      }
    }

    if (int.values.contains("maximum")) {
      try {
        props.add(MaxIntValueProperty(Some((int \ "maximum").extract[BigInt])))
      } catch {
        // $COVERAGE-OFF$
        case e: org.json4s.MappingException =>
        // $COVERAGE-ON$
      }
    }

    if (int.values.contains("exclusiveMaximum")) {
      try {
        props.add(
          MaxIntValueProperty(
            Some((int \ "exclusiveMaximum").extract[BigInt]),
            true
          )
        )
      } catch {
        // $COVERAGE-OFF$
        case e: org.json4s.MappingException =>
        // $COVERAGE-ON$
      }
    }

    if (int.values.contains("examples")) {
      try {
        val examples = (int \ "examples").extract[List[BigInt]]
        props.add(
          IntExamplesProperty(ExamplesProperty(examples, examples.length))
        )
      } catch {
        // $COVERAGE-OFF$
        case e: org.json4s.MappingException =>
        // $COVERAGE-ON$
      }
    }

    if (int.values.contains("bloomFilter")) {
      try {
        val bloomStr = (int \ "bloomFilter").extract[String]
        val bloomFilter = BloomFilter.deserialize[Integer](bloomStr)
        props.add(IntBloomFilterProperty(bloomFilter))
      } catch {
        // $COVERAGE-OFF$
        case e: org.json4s.MappingException        =>
        case e: java.io.EOFException               =>
        case e: java.io.StreamCorruptedException   =>
        case e: java.lang.IllegalArgumentException =>
        // $COVERAGE-ON$
      }
    }

    IntegerSchema(props)
  }

  lazy val AllProperties: SchemaProperties[BigInt] = {
    val props = SchemaProperties.empty[BigInt]
    props.add(MinIntValueProperty())
    props.add(MaxIntValueProperty())
    props.add(IntHyperLogLogProperty())
    props.add(IntBloomFilterProperty())
    props.add(IntStatsProperty())
    props.add(IntExamplesProperty())
    props.add(IntMultipleOfProperty())
    props.add(IntHistogramProperty())

    props
  }

  lazy val MinProperties: SchemaProperties[BigInt] = {
    SchemaProperties.empty[BigInt]
  }

  lazy val SimpleProperties: SchemaProperties[BigInt] = {
    val props = SchemaProperties.empty[BigInt]
    props.add(MinIntValueProperty())
    props.add(MaxIntValueProperty())
    props.add(IntMultipleOfProperty())
    props.add(IntExamplesProperty())

    props
  }
}

/** Represents integers in JSON Schema.
  */
final case class IntegerSchema(
    override val properties: SchemaProperties[BigInt] =
      IntegerSchema.AllProperties
) extends JsonSchema[BigInt] {
  override val schemaType = "integer"

  override val validTypes: Set[Class[_]] = Set(classOf[JInt])

  override def mergeSameType(mergeType: MergeType)(implicit
      p: JsonoidParams
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ IntegerSchema(otherProperties) =>
      IntegerSchema(properties.merge(otherProperties, mergeType))
    case other: NumberSchema => other.mergeSameType(mergeType)(p)(this)
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def copy(properties: SchemaProperties[BigInt]): IntegerSchema = {
    val newSchema = IntegerSchema(properties)
    newSchema.definitions ++= this.definitions

    newSchema
  }

  def asNumberSchema: NumberSchema = {
    val props = SchemaProperties.empty[BigDecimal]
    properties.foreach { prop =>
      prop match {
        case MinIntValueProperty(minValue, exclusive) =>
          props.add(MinNumValueProperty(minValue.map(_.toDouble), exclusive))
        case MaxIntValueProperty(maxValue, exclusive) =>
          props.add(MaxNumValueProperty(maxValue.map(_.toDouble), exclusive))
        case IntHyperLogLogProperty(hll) =>
          // XXX This can give some false positives due to how
          //     decimal values are tracked, but should not be
          //     a problem unless integer values are several
          //     orders of magnitude larger
          props.add(NumHyperLogLogProperty(hll))
        case IntBloomFilterProperty(bloomFilter) =>
          props.add(
            NumBloomFilterProperty(
              bloomFilter.asInstanceOf[BloomFilter[Double]]
            )
          )
        case IntStatsProperty(stats) =>
          props.add(NumStatsProperty(stats))
        case IntExamplesProperty(examples) =>
          props.add(
            NumExamplesProperty(
              ExamplesProperty[BigDecimal](
                examples.examples.map(BigDecimal(_)),
                examples.totalExamples,
                examples.nextSample,
                examples.sampleW
              )
            )
          )
        case IntHistogramProperty(histogram) =>
          props.add(NumHistogramProperty(histogram))
        case IntMultipleOfProperty(multiple) =>
          props.add(NumMultipleOfProperty(multiple.map(_.toDouble)))
      }
    }

    NumberSchema(props)
  }

  override def isSubsetOf(
      other: JsonSchema[_],
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    // Number schemas may be compatible with number schemas so try conversion
    if (other.isInstanceOf[NumberSchema])
      this.asNumberSchema.isSubsetOf(other, recursive)(p)
    else
      super.isSubsetOf(other, recursive)(p)
  }
}

/** Tracks the minimum value of all integers.
  *
  * @constructor Create a new minimum integer value property
  * @param minIntValue the minimum value
  * @param exclusive whether the minimum value is exclusive
  */
final case class MinIntValueProperty(
    minIntValue: Option[BigInt] = None,
    exclusive: Boolean = false
) extends SchemaProperty[BigInt] {
  override type S = MinIntValueProperty

  override def newDefault()(implicit p: JsonoidParams): MinIntValueProperty =
    MinIntValueProperty()

  override def toJson()(implicit p: JsonoidParams): JObject =
    ((if (exclusive) "exclusiveMinimum" else "minimum") -> minIntValue)

  override def intersectMerge(
      otherProp: MinIntValueProperty
  )(implicit p: JsonoidParams): MinIntValueProperty = {
    val exclusive = (minIntValue, otherProp.minIntValue) match {
      case (None, _)                   => this.exclusive
      case (_, None)                   => otherProp.exclusive
      case (Some(x), Some(y)) if x > y => this.exclusive
      case (Some(x), Some(y)) if x === y =>
        this.exclusive || otherProp.exclusive
      case _ => otherProp.exclusive
    }
    MinIntValueProperty(
      maxOrNone(minIntValue, otherProp.minIntValue),
      exclusive
    )
  }

  override def unionMerge(
      otherProp: MinIntValueProperty
  )(implicit p: JsonoidParams): MinIntValueProperty = {
    val exclusive = (minIntValue, otherProp.minIntValue) match {
      case (None, _)                   => this.exclusive
      case (_, None)                   => otherProp.exclusive
      case (Some(x), Some(y)) if x < y => this.exclusive
      case (Some(x), Some(y)) if x === y =>
        this.exclusive && otherProp.exclusive
      case _ => otherProp.exclusive
    }
    MinIntValueProperty(
      minOrNone(minIntValue, otherProp.minIntValue),
      exclusive
    )
  }

  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  override def mergeValue(
      value: BigInt
  )(implicit p: JsonoidParams): MinIntValueProperty = {
    val exclusive =
      this.exclusive && (minIntValue.isEmpty || value < minIntValue.get)
    MinIntValueProperty(minOrNone(Some(value), minIntValue))
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    value match {
      case JInt(num) =>
        minIntValue match {
          case Some(min) =>
            if (num <= min && exclusive) {
              Seq(
                Anomaly(
                  path,
                  "value is equal or below minimum",
                  AnomalyLevel.Warning
                )
              )
            } else if (num < min) {
              Seq(Anomaly(path, "value is below minimum", AnomalyLevel.Warning))
            } else {
              Seq.empty
            }
          case None => Seq.empty
        }
      case _ => Seq.empty
    }
  }

  override def isSubsetOf(
      other: MinIntValueProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    Helpers.isMinCoveredBy(
      minIntValue,
      exclusive,
      other.minIntValue,
      other.exclusive
    )
  }

  override def expandTo(
      other: Option[MinIntValueProperty]
  ): MinIntValueProperty = {
    val (newMin, newExclusive) = maybeContractInt(
      minIntValue.map(_.toInt),
      other
        .map(o => o.minIntValue.map(i => i.toInt + (if (o.exclusive) 1 else 0)))
        .getOrElse(None),
      exclusive,
      other.isEmpty
    )
    MinIntValueProperty(newMin.map(BigInt(_)), newExclusive)
  }
}

/** Tracks the maximum value of all integers.
  *
  * @constructor Create a new maximum integer value property
  * @param maxIntValue the maximum value
  * @param exclusive whether the maximum value is exclusive
  */
final case class MaxIntValueProperty(
    maxIntValue: Option[BigInt] = None,
    exclusive: Boolean = false
) extends SchemaProperty[BigInt] {
  override type S = MaxIntValueProperty

  override def newDefault()(implicit p: JsonoidParams): MaxIntValueProperty =
    MaxIntValueProperty()

  override def toJson()(implicit p: JsonoidParams): JObject =
    ((if (exclusive) "exclusiveMaximum" else "maximum") -> maxIntValue)

  override def intersectMerge(
      otherProp: MaxIntValueProperty
  )(implicit p: JsonoidParams): MaxIntValueProperty = {
    val exclusive = (maxIntValue, otherProp.maxIntValue) match {
      case (None, _)                   => this.exclusive
      case (_, None)                   => otherProp.exclusive
      case (Some(x), Some(y)) if x < y => this.exclusive
      case (Some(x), Some(y)) if x === y =>
        this.exclusive || otherProp.exclusive
      case _ => otherProp.exclusive
    }
    MaxIntValueProperty(
      minOrNone(maxIntValue, otherProp.maxIntValue),
      exclusive
    )
  }

  override def unionMerge(
      otherProp: MaxIntValueProperty
  )(implicit p: JsonoidParams): MaxIntValueProperty = {
    val exclusive = (maxIntValue, otherProp.maxIntValue) match {
      case (None, _)                   => this.exclusive
      case (_, None)                   => otherProp.exclusive
      case (Some(x), Some(y)) if x > y => this.exclusive
      case (Some(x), Some(y)) if x === y =>
        this.exclusive && otherProp.exclusive
      case _ => otherProp.exclusive
    }
    MaxIntValueProperty(
      maxOrNone(maxIntValue, otherProp.maxIntValue),
      exclusive
    )
  }

  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  override def mergeValue(
      value: BigInt
  )(implicit p: JsonoidParams): MaxIntValueProperty = {
    val exclusive =
      this.exclusive && (maxIntValue.isEmpty || value < maxIntValue.get)
    MaxIntValueProperty(maxOrNone(Some(value), maxIntValue), exclusive)
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    value match {
      case JInt(num) =>
        maxIntValue match {
          case Some(max) =>
            if (num >= max && exclusive) {
              Seq(
                Anomaly(
                  path,
                  "value is equal or above maximum",
                  AnomalyLevel.Warning
                )
              )
            } else if (num > max) {
              Seq(Anomaly(path, "value is above maximum", AnomalyLevel.Warning))
            } else {
              Seq.empty
            }
          case None => Seq.empty
        }
      case _ => Seq.empty
    }
  }

  override def isSubsetOf(
      other: MaxIntValueProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    Helpers.isMaxCoveredBy(
      maxIntValue,
      exclusive,
      other.maxIntValue,
      other.exclusive
    )
  }

  override def expandTo(
      other: Option[MaxIntValueProperty]
  ): MaxIntValueProperty = {
    val (newMax, newExclusive) = maybeExpandInt(
      maxIntValue,
      other
        .map(o => o.maxIntValue.map(i => i - (if (o.exclusive) 1 else 0)))
        .getOrElse(None),
      exclusive,
      other.isEmpty
    )
    MaxIntValueProperty(newMax, newExclusive)
  }
}

/** Tracks the estimated cardinality of the set of integer values.
  *
  * @constructor Create a new HLL property
  * @param hll the HyperLogLog data structure used to track the cardinality
  */
final case class IntHyperLogLogProperty(
    hll: HyperLogLog = new HyperLogLog()
) extends SchemaProperty[BigInt] {
  override type S = IntHyperLogLogProperty

  override def newDefault()(implicit p: JsonoidParams): IntHyperLogLogProperty =
    IntHyperLogLogProperty()

  override val isInformational = true

  override def toJson()(implicit p: JsonoidParams): JObject =
    ("distinctValues" -> hll.count()) ~ ("hll" -> hll.toBase64)

  override def unionMerge(
      otherProp: IntHyperLogLogProperty
  )(implicit p: JsonoidParams): IntHyperLogLogProperty = {
    val prop = IntHyperLogLogProperty()
    prop.hll.merge(this.hll)
    prop.hll.merge(otherProp.hll)

    prop
  }

  override def mergeValue(
      value: BigInt
  )(implicit p: JsonoidParams): IntHyperLogLogProperty = {
    val prop = IntHyperLogLogProperty()
    prop.hll.merge(this.hll)
    prop.hll.add(value.toLong)

    prop
  }
}

/** Tracks possible integers which can be contained in the set
  *
  * @constructor Create a new integer Bloom filter property
  * @param bloomFilter the Bloom filter used to track the set of integers
  */
final case class IntBloomFilterProperty(
    bloomFilter: BloomFilter[Integer] = BloomFilter[Integer]()
) extends SchemaProperty[BigInt] {
  override type S = IntBloomFilterProperty

  override def newDefault()(implicit p: JsonoidParams): IntBloomFilterProperty =
    IntBloomFilterProperty()

  override val isInformational = true

  override def toJson()(implicit p: JsonoidParams): JObject = {
    ("bloomFilter" -> bloomFilter.toBase64)
  }

  override def unionMerge(
      otherProp: IntBloomFilterProperty
  )(implicit p: JsonoidParams): IntBloomFilterProperty = {
    val prop = IntBloomFilterProperty()
    prop.bloomFilter.filter.merge(this.bloomFilter.filter)
    prop.bloomFilter.filter.merge(otherProp.bloomFilter.filter)

    prop
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def mergeValue(
      value: BigInt
  )(implicit p: JsonoidParams): IntBloomFilterProperty = {
    val prop = IntBloomFilterProperty()
    prop.bloomFilter.filter.merge(this.bloomFilter.filter)
    prop.bloomFilter.filter.add(value.toByteArray)

    prop
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    val inFilter = value match {
      case JInt(num) => Some(bloomFilter.contains(num.toByteArray))
      case _         => None
    }

    inFilter match {
      case Some(false) =>
        Seq(Anomaly(path, "value not found in Bloom filter", AnomalyLevel.Info))
      case _ => Seq.empty
    }
  }
}

/** Tracks statistics on the integers in this set
  *
  * @constructor Create a new integer statistics property
  * @param stats the statistics used to track the set of integers
  */
final case class IntStatsProperty(stats: StatsProperty = StatsProperty())
    extends SchemaProperty[BigInt] {
  override type S = IntStatsProperty

  override def newDefault()(implicit p: JsonoidParams): IntStatsProperty =
    IntStatsProperty()

  override val isInformational = true

  override def toJson()(implicit p: JsonoidParams): JObject =
    ("statistics" -> stats.toJson)

  override def unionMerge(
      otherProp: IntStatsProperty
  )(implicit p: JsonoidParams): IntStatsProperty = {
    IntStatsProperty(stats.merge(otherProp.stats))
  }

  override def mergeValue(
      value: BigInt
  )(implicit p: JsonoidParams): IntStatsProperty = {
    IntStatsProperty(stats.merge(StatsProperty(BigDecimal(value))))
  }
}

/** Tracks examples observed for these integers.
  *
  * @constructor Create a new integer examples property
  * @param examples the example integers observed
  */
final case class IntExamplesProperty(
    examples: ExamplesProperty[BigInt] = ExamplesProperty()
) extends SchemaProperty[BigInt] {
  override type S = IntExamplesProperty

  override def newDefault()(implicit p: JsonoidParams): IntExamplesProperty =
    IntExamplesProperty()

  override val isInformational = true

  override def toJson()(implicit p: JsonoidParams): JObject = ("examples" ->
    examples.examples.distinct.sorted)

  override def unionMerge(
      otherProp: IntExamplesProperty
  )(implicit p: JsonoidParams): IntExamplesProperty = {
    IntExamplesProperty(examples.merge(otherProp.examples))
  }

  override def mergeValue(
      value: BigInt
  )(implicit p: JsonoidParams): IntExamplesProperty = {
    IntExamplesProperty(examples.merge(ExamplesProperty(value)))
  }
}

/** Tracks a common multiple of these integers.
  *
  * @constructor Create a new integer multiple property
  * @param multiple a possible common multiple of the integers
  */
final case class IntMultipleOfProperty(multiple: Option[BigInt] = None)
    extends SchemaProperty[BigInt] {
  override type S = IntMultipleOfProperty

  override def newDefault()(implicit p: JsonoidParams): IntMultipleOfProperty =
    IntMultipleOfProperty()

  override def toJson()(implicit p: JsonoidParams): JObject = multiple match {
    case Some(intVal) if intVal > 1 => ("multipleOf" -> intVal)
    case _                          => Nil
  }

  override def intersectMerge(
      otherProp: IntMultipleOfProperty
  )(implicit p: JsonoidParams): IntMultipleOfProperty = {
    val newMultiple = (multiple, otherProp.multiple) match {
      case (Some(m), None)    => Some(m)
      case (None, Some(n))    => Some(n)
      case (Some(m), Some(n)) => Some(lcm(m, n))
      case (None, None)       => None
    }
    IntMultipleOfProperty(newMultiple)
  }

  override def unionMerge(
      otherProp: IntMultipleOfProperty
  )(implicit p: JsonoidParams): IntMultipleOfProperty = {
    val newMultiple = (multiple, otherProp.multiple) match {
      case (Some(m), None)    => Some(m)
      case (None, Some(n))    => Some(n)
      case (Some(m), Some(n)) => Some(gcd(m, n))
      case (None, None)       => None
    }
    IntMultipleOfProperty(newMultiple)
  }

  override def mergeValue(
      value: BigInt
  )(implicit p: JsonoidParams): IntMultipleOfProperty = {
    unionMerge(IntMultipleOfProperty(Some(value)))
  }

  @SuppressWarnings(
    Array("org.wartremover.warts.OptionPartial")
  )
  override def isSubsetOf(
      other: IntMultipleOfProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    if (other.multiple.isEmpty) {
      // If the other schema has no multiple, then compatible
      true
    } else if (multiple.isEmpty) {
      // If the other schema has a multiple and we don't, not compatible
      false
    } else {
      // Otherwise, our multiple must be a multiple of the other multiple
      if (other.multiple.get === 0) {
        multiple.get === 0
      } else {
        multiple.get % other.multiple.get === 0
      }
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def expandTo(
      other: Option[IntMultipleOfProperty]
  ): IntMultipleOfProperty = {
    // Assume we have no multiple if the other prop is not specified
    (multiple, other.map(_.multiple).getOrElse(None)) match {
      case (Some(mult), Some(otherMult)) =>
        // Try removing the smallest prime factor
        val newMult = (1 to MaxExpandRounds)
          .scanLeft(mult.toInt) { (oldMult: Int, _) =>
            if (oldMult == 1 || oldMult == 0) {
              // If we're down to one or zero, we have to stop
              0
            } else {
              oldMult / factorize(oldMult).sorted.headOption.getOrElse(1)
            }
          }
          .find(m => m == 0 || (m != 1 && otherMult % m === 0))

        // New multiple must not be larger if it exists
        assert(BigInt(newMult.getOrElse(0)) <= mult)

        newMult match {
          case Some(0) => IntMultipleOfProperty(None)
          case _       => IntMultipleOfProperty(newMult.map(BigInt(_)))
        }
      case (_, None) => IntMultipleOfProperty(None)
      case (None, _) => this
    }
  }
}

/** Tracks a histogram of integer values.
  *
  * @constructor Create a new integer histogram property
  * @param histogram a histogram of integer values
  */
final case class IntHistogramProperty(histogram: Histogram = Histogram())
    extends SchemaProperty[BigInt] {
  override type S = IntHistogramProperty

  override def newDefault()(implicit p: JsonoidParams): IntHistogramProperty =
    IntHistogramProperty()

  override val isInformational = true

  override def toJson()(implicit p: JsonoidParams): JObject = {
    ("histogram" -> histogram.toJson)
  }

  override def unionMerge(
      otherProp: IntHistogramProperty
  )(implicit p: JsonoidParams): IntHistogramProperty = {
    IntHistogramProperty(histogram.merge(otherProp.histogram))
  }

  override def mergeValue(
      value: BigInt
  )(implicit p: JsonoidParams): IntHistogramProperty = {
    IntHistogramProperty(histogram.merge(value))
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    value match {
      case JInt(num) =>
        if (histogram.isAnomalous(num.doubleValue)) {
          Seq(
            Anomaly(
              path,
              "value outside histogram bounds",
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

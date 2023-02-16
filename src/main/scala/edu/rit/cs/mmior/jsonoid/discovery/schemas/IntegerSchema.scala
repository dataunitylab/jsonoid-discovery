package edu.rit.cs.mmior.jsonoid.discovery
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
  )(implicit propSet: PropertySet, p: JsonoidParams): IntegerSchema = {
    IntegerSchema(
      propSet.integerProperties.mergeValue(value)(p)
    )
  }

  val AllProperties: SchemaProperties[BigInt] = {
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

  val MinProperties: SchemaProperties[BigInt] = {
    SchemaProperties.empty[BigInt]
  }

  val SimpleProperties: SchemaProperties[BigInt] = {
    val props = SchemaProperties.empty[BigInt]
    props.add(MinIntValueProperty())
    props.add(MaxIntValueProperty())
    props.add(IntMultipleOfProperty())
    props.add(IntExamplesProperty())

    props
  }
}

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
          NumMultipleOfProperty(multiple.map(_.toDouble))
      }
    }

    NumberSchema(props)
  }
}

final case class MinIntValueProperty(
    minIntValue: Option[BigInt] = None,
    exclusive: Boolean = false
) extends SchemaProperty[BigInt] {
  override type S = MinIntValueProperty

  override def newDefault: MinIntValueProperty = MinIntValueProperty()

  override def toJson()(implicit p: JsonoidParams): JObject =
    ((if (exclusive) { "exclusiveMinimum" }
      else { "minimum" }) -> minIntValue)

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
      tag: ClassTag[S]
  ) = {
    value match {
      case JInt(num) =>
        minIntValue match {
          case Some(min) =>
            if (num <= min && exclusive) {
              Seq(Anomaly(path, "value is equal or below minimum", Warning))
            } else if (num < min) {
              Seq(Anomaly(path, "value is below minimum", Warning))
            } else {
              Seq.empty
            }
          case None => Seq.empty
        }
      case _ => Seq.empty
    }
  }

  override def isCompatibleWith(
      other: MinIntValueProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    Helpers.isMinCompatibleWith(
      minIntValue,
      exclusive,
      other.minIntValue,
      other.exclusive
    )
  }

  override def expandTo(other: MinIntValueProperty): MinIntValueProperty = {
    val (newMin, newExclusive) = maybeContractInt(
      minIntValue.map(_.toInt),
      other.minIntValue.map(_.toInt + (if (other.exclusive) 1 else 0)),
      exclusive
    )
    MinIntValueProperty(newMin.map(BigInt(_)), newExclusive)
  }
}

final case class MaxIntValueProperty(
    maxIntValue: Option[BigInt] = None,
    exclusive: Boolean = false
) extends SchemaProperty[BigInt] {
  override type S = MaxIntValueProperty

  override def newDefault: MaxIntValueProperty = MaxIntValueProperty()

  override def toJson()(implicit p: JsonoidParams): JObject =
    ((if (exclusive) { "exclusiveMaximum" }
      else { "maximum" }) -> maxIntValue)

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
      tag: ClassTag[S]
  ) = {
    value match {
      case JInt(num) =>
        maxIntValue match {
          case Some(max) =>
            if (num >= max && exclusive) {
              Seq(Anomaly(path, "value is equal or above maximum", Warning))
            } else if (num > max) {
              Seq(Anomaly(path, "value is above maximum", Warning))
            } else {
              Seq.empty
            }
          case None => Seq.empty
        }
      case _ => Seq.empty
    }
  }

  override def isCompatibleWith(
      other: MaxIntValueProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    Helpers.isMaxCompatibleWith(
      maxIntValue,
      exclusive,
      other.maxIntValue,
      other.exclusive
    )
  }

  override def expandTo(other: MaxIntValueProperty): MaxIntValueProperty = {
    val (newMax, newExclusive) = maybeExpandInt(
      maxIntValue.map(_.toInt),
      other.maxIntValue.map(_.toInt - (if (other.exclusive) 1 else 0)),
      exclusive
    )
    MaxIntValueProperty(newMax.map(BigInt(_)), newExclusive)
  }
}

final case class IntHyperLogLogProperty(
    hll: HyperLogLog = new HyperLogLog()
) extends SchemaProperty[BigInt] {
  override type S = IntHyperLogLogProperty

  override def newDefault: IntHyperLogLogProperty = IntHyperLogLogProperty()

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

final case class IntBloomFilterProperty(
    bloomFilter: BloomFilter[Integer] = BloomFilter[Integer]()
) extends SchemaProperty[BigInt] {
  override type S = IntBloomFilterProperty

  override def newDefault: IntBloomFilterProperty = IntBloomFilterProperty()

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
      tag: ClassTag[S]
  ) = {
    val inFilter = value match {
      case JInt(num) => Some(bloomFilter.contains(num.toByteArray))
      case _         => None
    }

    inFilter match {
      case Some(false) =>
        Seq(Anomaly(path, "value not found in Bloom filter", Info))
      case _ => Seq.empty
    }
  }
}

final case class IntStatsProperty(stats: StatsProperty = StatsProperty())
    extends SchemaProperty[BigInt] {
  override type S = IntStatsProperty

  override def newDefault: IntStatsProperty = IntStatsProperty()

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

final case class IntExamplesProperty(
    examples: ExamplesProperty[BigInt] = ExamplesProperty()
) extends SchemaProperty[BigInt] {
  override type S = IntExamplesProperty

  override def newDefault: IntExamplesProperty = IntExamplesProperty()

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

final case class IntMultipleOfProperty(multiple: Option[BigInt] = None)
    extends SchemaProperty[BigInt] {
  override type S = IntMultipleOfProperty

  override def newDefault: IntMultipleOfProperty = IntMultipleOfProperty()

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
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
    Array("org.wartremover.warts.Equals", "org.wartremover.warts.OptionPartial")
  )
  override def isCompatibleWith(
      other: IntMultipleOfProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    if (multiple.isEmpty) {
      // If we have no multiple, then compatible
      true
    } else if (other.multiple.isEmpty) {
      // If we have a multiple and the other schema doesn't, not compatible
      false
    } else {
      // Otherwise, the multiple must be a multiple
      // of our multiple with the same sign
      (other.multiple.get == 0 && multiple.get == 0) ||
      (multiple.get != 0 && other.multiple.get % multiple.get == 0 &&
        multiple.get.signum == other.multiple.get.signum)
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def expandTo(other: IntMultipleOfProperty): IntMultipleOfProperty = {
    (multiple, other.multiple) match {
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

        newMult match {
          case Some(0) => IntMultipleOfProperty(None)
          case _       => IntMultipleOfProperty(newMult.map(BigInt(_)))
        }
      case (_, None) => IntMultipleOfProperty(None)
      case (None, _) => this
    }
  }
}

final case class IntHistogramProperty(histogram: Histogram = Histogram())
    extends SchemaProperty[BigInt] {
  override type S = IntHistogramProperty

  override def newDefault: IntHistogramProperty = IntHistogramProperty()

  override val isInformational = true

  override def toJson()(implicit p: JsonoidParams): JObject = {
    ("histogram" -> histogram.bins.map { case (value, count) =>
      List(value, count)
    })
  }

  override def unionMerge(
      otherProp: IntHistogramProperty
  )(implicit p: JsonoidParams): IntHistogramProperty = {
    IntHistogramProperty(histogram.merge(otherProp.histogram))
  }

  override def mergeValue(
      value: BigInt
  )(implicit p: JsonoidParams): IntHistogramProperty = {
    IntHistogramProperty(histogram.merge(value.doubleValue))
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      tag: ClassTag[S]
  ) = {
    value match {
      case JInt(num) =>
        if (histogram.isAnomalous(num.doubleValue)) {
          Seq(Anomaly(path, "value outside histogram bounds", Warning))
        } else {
          Seq.empty
        }
      case _ => Seq.empty
    }
  }
}

package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

import java.io.{ByteArrayOutputStream, ObjectOutputStream}
import java.util.Base64

import scala.reflect._
import scala.util.Try

import scalaz._
import org.json4s.JsonDSL._
import org.json4s._
import Scalaz._

import Helpers._
import utils.{BloomFilter, Histogram, HyperLogLog}

object NumberSchema {
  def apply(
      value: BigDecimal
  )(implicit p: JsonoidParams): NumberSchema = {
    NumberSchema(
      p.propSet.numberProperties.mergeValue(value)(p)
    )
  }

  /** Convert a serialized JSON value to a number schema object. */
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def fromJson(num: JObject): NumberSchema = {
    implicit val formats: Formats = DefaultFormats
    val props = SchemaProperties.empty[BigDecimal]

    if ((num \ "multipleOf") != JNothing) {
      props.add(
        NumMultipleOfProperty(Some((num \ "multipleOf").extract[BigDecimal]))
      )
    }

    if ((num \ "minimum") != JNothing) {
      props.add(
        MinNumValueProperty(Some((num \ "minimum").extract[BigDecimal]))
      )
    }

    if ((num \ "exclusiveMinimum") != JNothing) {
      props.add(
        MinNumValueProperty(
          Some((num \ "exclusiveMinimum").extract[BigDecimal]),
          true
        )
      )
    }

    if ((num \ "maximum") != JNothing) {
      props.add(
        MaxNumValueProperty(Some((num \ "maximum").extract[BigDecimal]))
      )
    }

    if ((num \ "exclusiveMaximum") != JNothing) {
      props.add(
        MaxNumValueProperty(
          Some((num \ "exclusiveMaximum").extract[BigDecimal]),
          true
        )
      )
    }

    if (num.values.contains("examples")) {
      val examples = (num \ "examples").extract[List[BigDecimal]]
      props.add(
        NumExamplesProperty(ExamplesProperty(examples, examples.length))
      )
    }

    if (num.values.contains("bloomFilter")) {
      val bloomStr = (num \ "bloomFilter").extract[String]
      val bloomFilter = BloomFilter.deserialize[Double](bloomStr)
      props.add(NumBloomFilterProperty(bloomFilter))
    }

    NumberSchema(props)
  }

  val AllProperties: SchemaProperties[BigDecimal] = {
    val props = SchemaProperties.empty[BigDecimal]
    props.add(MinNumValueProperty())
    props.add(MaxNumValueProperty())
    props.add(NumHyperLogLogProperty())
    props.add(NumBloomFilterProperty())
    props.add(NumMultipleOfProperty())
    props.add(NumStatsProperty())
    props.add(NumExamplesProperty())
    props.add(NumHistogramProperty())

    props
  }

  val MinProperties: SchemaProperties[BigDecimal] = {
    SchemaProperties.empty[BigDecimal]
  }

  val SimpleProperties: SchemaProperties[BigDecimal] = {
    val props = SchemaProperties.empty[BigDecimal]
    props.add(MinNumValueProperty())
    props.add(MaxNumValueProperty())
    props.add(NumMultipleOfProperty())
    props.add(NumExamplesProperty())

    props
  }
}

/** Represents numbers in JSON Schema.
  */
final case class NumberSchema(
    override val properties: SchemaProperties[BigDecimal] =
      NumberSchema.AllProperties
) extends JsonSchema[BigDecimal] {
  override val schemaType = "number"

  override val validTypes: Set[Class[_]] =
    Set(classOf[JInt], classOf[JDouble], classOf[JDecimal])

  @SuppressWarnings(
    Array("org.wartremover.warts.Recursion", "org.wartremover.warts.Var")
  )
  override def mergeSameType(mergeType: MergeType)(implicit
      p: JsonoidParams
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ NumberSchema(otherProperties) =>
      NumberSchema(properties.merge(otherProperties, mergeType))

    case other @ IntegerSchema(otherProperties) => {
      mergeSameType(mergeType)(p)(other.asNumberSchema)
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def copy(properties: SchemaProperties[BigDecimal]): NumberSchema = {
    val newSchema = NumberSchema(properties)
    newSchema.definitions ++= this.definitions

    newSchema
  }

  override def isCompatibleWith(
      other: JsonSchema[_],
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    // Integer schemas may be compatible with number schemas so try conversion
    if (other.isInstanceOf[IntegerSchema]) {
      super.isCompatibleWith(
        other.asInstanceOf[IntegerSchema].asNumberSchema,
        recursive
      )(p)
    } else {
      super.isCompatibleWith(other, recursive)(p)
    }
  }
}

/** Tracks the minimum value of all numbers.
  *
  * @constructor Create a new minimum numbers value property
  * @param minNumValue the minimum value
  * @param exclusive whether the minimum value is exclusive
  */
final case class MinNumValueProperty(
    minNumValue: Option[BigDecimal] = None,
    exclusive: Boolean = false
) extends SchemaProperty[BigDecimal] {
  override type S = MinNumValueProperty

  override def newDefault()(implicit p: JsonoidParams): MinNumValueProperty =
    MinNumValueProperty()

  override def toJson()(implicit p: JsonoidParams): JObject =
    ((if (exclusive) { "exclusiveMinimum" }
      else { "minimum" }) -> minNumValue)

  override def intersectMerge(
      otherProp: MinNumValueProperty
  )(implicit p: JsonoidParams): MinNumValueProperty = {
    val exclusive = (minNumValue, otherProp.minNumValue) match {
      case (None, _)                   => this.exclusive
      case (_, None)                   => otherProp.exclusive
      case (Some(x), Some(y)) if x > y => this.exclusive
      case (Some(x), Some(y)) if x === y =>
        this.exclusive || otherProp.exclusive
      case _ => otherProp.exclusive
    }
    MinNumValueProperty(
      maxOrNone(minNumValue, otherProp.minNumValue),
      exclusive
    )
  }

  override def unionMerge(
      otherProp: MinNumValueProperty
  )(implicit p: JsonoidParams): MinNumValueProperty = {
    val exclusive = (minNumValue, otherProp.minNumValue) match {
      case (None, _)                   => this.exclusive
      case (_, None)                   => otherProp.exclusive
      case (Some(x), Some(y)) if x < y => this.exclusive
      case (Some(x), Some(y)) if x === y =>
        this.exclusive && otherProp.exclusive
      case _ => otherProp.exclusive
    }
    MinNumValueProperty(
      minOrNone(minNumValue, otherProp.minNumValue),
      exclusive
    )
  }

  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  override def mergeValue(
      value: BigDecimal
  )(implicit p: JsonoidParams): MinNumValueProperty = {
    val exclusive =
      this.exclusive && (minNumValue.isEmpty || value < minNumValue.get)
    MinNumValueProperty(minOrNone(Some(value), minNumValue))
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    minNumValue match {
      case Some(min) =>
        val exceedsMin = value match {
          case JDouble(num) if exclusive   => Some(num <= min)
          case JDouble(num) if !exclusive  => Some(num < min)
          case JDecimal(num) if exclusive  => Some(num <= min)
          case JDecimal(num) if !exclusive => Some(num < min)
          case JInt(num) if exclusive      => Some(BigDecimal(num) <= min)
          case JInt(num) if !exclusive     => Some(BigDecimal(num) < min)
          case _                           => None
        }

        exceedsMin match {
          case Some(true) if exclusive =>
            Seq(
              Anomaly(
                path,
                "value is equal or below minimum",
                AnomalyLevel.Warning
              )
            )
          case Some(true) if !exclusive =>
            Seq(Anomaly(path, "value is below minimum", AnomalyLevel.Warning))
          case _ => Seq.empty
        }
      case None => Seq.empty
    }
  }

  override def isCompatibleWith(
      other: MinNumValueProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    Helpers.isMinCompatibleWith(
      minNumValue,
      exclusive,
      other.minNumValue,
      other.exclusive
    )
  }

  override def expandTo(
      other: Option[MinNumValueProperty]
  ): MinNumValueProperty = {
    val (newMin, newExclusive) = maybeContractInt(
      minNumValue.map(_.toInt),
      other
        .map(o => o.minNumValue.map(i => i.toInt + (if (o.exclusive) 1 else 0)))
        .getOrElse(None),
      exclusive,
      other.isEmpty
    )
    MinNumValueProperty(newMin.map(BigDecimal(_)), newExclusive)
  }
}

/** Tracks the maximum value of all numbers.
  *
  * @constructor Create a new maximum numbers value property
  * @param maxNumValue the maximum value
  * @param exclusive whether the maximum value is exclusive
  */
final case class MaxNumValueProperty(
    maxNumValue: Option[BigDecimal] = None,
    exclusive: Boolean = false
) extends SchemaProperty[BigDecimal] {
  override type S = MaxNumValueProperty

  override def newDefault()(implicit p: JsonoidParams): MaxNumValueProperty =
    MaxNumValueProperty()

  override def toJson()(implicit p: JsonoidParams): JObject =
    ((if (exclusive) { "exclusiveMaximum" }
      else { "maximum" }) -> maxNumValue)

  override def intersectMerge(
      otherProp: MaxNumValueProperty
  )(implicit p: JsonoidParams): MaxNumValueProperty = {
    val exclusive = (maxNumValue, otherProp.maxNumValue) match {
      case (None, _)                   => this.exclusive
      case (_, None)                   => otherProp.exclusive
      case (Some(x), Some(y)) if x < y => this.exclusive
      case (Some(x), Some(y)) if x === y =>
        this.exclusive || otherProp.exclusive
      case _ => otherProp.exclusive
    }
    MaxNumValueProperty(
      minOrNone(maxNumValue, otherProp.maxNumValue),
      exclusive
    )
  }

  override def unionMerge(
      otherProp: MaxNumValueProperty
  )(implicit p: JsonoidParams): MaxNumValueProperty = {
    val exclusive = (maxNumValue, otherProp.maxNumValue) match {
      case (None, _)                   => this.exclusive
      case (_, None)                   => otherProp.exclusive
      case (Some(x), Some(y)) if x > y => this.exclusive
      case (Some(x), Some(y)) if x === y =>
        this.exclusive && otherProp.exclusive
      case _ => otherProp.exclusive
    }
    MaxNumValueProperty(maxOrNone(maxNumValue, otherProp.maxNumValue))
  }

  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  override def mergeValue(
      value: BigDecimal
  )(implicit p: JsonoidParams): MaxNumValueProperty = {
    val exclusive =
      this.exclusive && (maxNumValue.isEmpty || value < maxNumValue.get)
    MaxNumValueProperty(maxOrNone(Some(value), maxNumValue), exclusive)
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    maxNumValue match {
      case Some(max) =>
        val exceedsMax = value match {
          case JDouble(num) if exclusive   => Some(num >= max)
          case JDouble(num) if !exclusive  => Some(num > max)
          case JDecimal(num) if exclusive  => Some(num >= max)
          case JDecimal(num) if !exclusive => Some(num > max)
          case JInt(num) if exclusive      => Some(BigDecimal(num) >= max)
          case JInt(num) if !exclusive     => Some(BigDecimal(num) > max)
          case _                           => None
        }

        exceedsMax match {
          case Some(true) if exclusive =>
            Seq(
              Anomaly(
                path,
                "value is equal or above maximum",
                AnomalyLevel.Warning
              )
            )
          case Some(true) if !exclusive =>
            Seq(Anomaly(path, "value is above maximum", AnomalyLevel.Warning))
          case _ => Seq.empty
        }
      case None => Seq.empty
    }
  }

  override def isCompatibleWith(
      other: MaxNumValueProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    Helpers.isMaxCompatibleWith(
      maxNumValue,
      exclusive,
      other.maxNumValue,
      other.exclusive
    )
  }
  override def expandTo(
      other: Option[MaxNumValueProperty]
  ): MaxNumValueProperty = {
    val (newMax, newExclusive) = maybeExpandInt(
      maxNumValue.map(_.setScale(0, BigDecimal.RoundingMode.FLOOR).toBigInt),
      other
        .map(o =>
          o.maxNumValue.map(n =>
            n.setScale(0, BigDecimal.RoundingMode.CEILING).toBigInt
          )
        )
        .getOrElse(None),
      exclusive,
      other.isEmpty
    )
    MaxNumValueProperty(newMax.map(BigDecimal(_)), newExclusive)
  }
}

/** Tracks the estimated cardinality of the set of number values.
  *
  * @constructor Create a new HLL property
  * @param hll the HyperLogLog data structure used to track the cardinality
  */
final case class NumHyperLogLogProperty(hll: HyperLogLog = new HyperLogLog())
    extends SchemaProperty[BigDecimal] {
  override type S = NumHyperLogLogProperty

  override def newDefault()(implicit p: JsonoidParams): NumHyperLogLogProperty =
    NumHyperLogLogProperty()

  override val isInformational = true

  override def toJson()(implicit p: JsonoidParams): JObject =
    ("distinctValues" -> hll.count()) ~ ("hll" ->
      hll.toBase64())

  override def unionMerge(
      otherProp: NumHyperLogLogProperty
  )(implicit p: JsonoidParams): NumHyperLogLogProperty = {
    val prop = NumHyperLogLogProperty()
    prop.hll.merge(this.hll)
    prop.hll.merge(otherProp.hll)

    prop
  }

  override def mergeValue(
      value: BigDecimal
  )(implicit p: JsonoidParams): NumHyperLogLogProperty = {
    val prop = NumHyperLogLogProperty()
    prop.hll.merge(this.hll)
    val longVal = if (value.isValidLong) {
      value.toLong
    } else {
      // XXX Use first five decimal places
      //     This could later conflict with a larger integer value
      //     i.e. 3.14159 will match 314159
      (value * 100000).toLong
    }
    prop.hll.add(longVal)

    prop
  }
}

/** Tracks possible numbers which can be contained in the set
  *
  * @constructor Create a new number Bloom filter property
  * @param bloomFilter the Bloom filter used to track the set of numbers
  */
final case class NumBloomFilterProperty(
    bloomFilter: BloomFilter[Double] = BloomFilter[Double]()
) extends SchemaProperty[BigDecimal] {
  override type S = NumBloomFilterProperty

  override def newDefault()(implicit p: JsonoidParams): NumBloomFilterProperty =
    NumBloomFilterProperty()

  override val isInformational = true

  override def toJson()(implicit p: JsonoidParams): JObject = {
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(bloomFilter)
    oos.close()

    val bloomStr = Base64.getEncoder().encodeToString(baos.toByteArray())
    ("bloomFilter" -> bloomStr)
  }

  override def unionMerge(
      otherProp: NumBloomFilterProperty
  )(implicit p: JsonoidParams): NumBloomFilterProperty = {
    val prop = NumBloomFilterProperty()
    prop.bloomFilter.filter.merge(this.bloomFilter.filter)
    prop.bloomFilter.filter.merge(otherProp.bloomFilter.filter)

    prop
  }

  def scaleValue(value: BigDecimal): Array[Byte] = {
    value.toBigIntExact match {
      case Some(int) => int.toByteArray
      case None      => value.toString.getBytes
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def mergeValue(
      value: BigDecimal
  )(implicit p: JsonoidParams): NumBloomFilterProperty = {
    val prop = NumBloomFilterProperty()
    prop.bloomFilter.filter.merge(this.bloomFilter.filter)

    val scaled = scaleValue(value)
    prop.bloomFilter.filter.add(scaled)

    prop
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    val inFilter = value match {
      case JDouble(num) =>
        Some(bloomFilter.contains(scaleValue(BigDecimal(num))))
      case JDecimal(num) => Some(bloomFilter.contains(scaleValue(num)))
      case JInt(num)     => Some(bloomFilter.contains(num.toByteArray))
      case _             => None
    }

    inFilter match {
      case Some(false) =>
        Seq(Anomaly(path, "value not found in Bloom filter", AnomalyLevel.Info))
      case _ => Seq.empty
    }
  }
}

/** Tracks statistics on the numbers in this set
  *
  * @constructor Create a new number statistics property
  * @param stats the statistics used to track the set of numbers
  */
final case class NumStatsProperty(stats: StatsProperty = StatsProperty())
    extends SchemaProperty[BigDecimal] {
  override type S = NumStatsProperty

  override def newDefault()(implicit p: JsonoidParams): NumStatsProperty =
    NumStatsProperty()

  override val isInformational = true

  override def toJson()(implicit p: JsonoidParams): JObject =
    ("statistics" -> stats.toJson)

  override def unionMerge(
      otherProp: NumStatsProperty
  )(implicit p: JsonoidParams): NumStatsProperty = {
    NumStatsProperty(stats.merge(otherProp.stats))
  }

  override def mergeValue(
      value: BigDecimal
  )(implicit p: JsonoidParams): NumStatsProperty = {
    NumStatsProperty(stats.merge(StatsProperty(value)))
  }
}

/** Tracks examples observed for these numbers.
  *
  * @constructor Create a new number examples property
  * @param examples the example numbers observed
  */
final case class NumExamplesProperty(
    examples: ExamplesProperty[BigDecimal] = ExamplesProperty()
) extends SchemaProperty[BigDecimal] {
  override type S = NumExamplesProperty

  override def newDefault()(implicit p: JsonoidParams): NumExamplesProperty =
    NumExamplesProperty()

  override val isInformational = true

  override def toJson()(implicit p: JsonoidParams): JObject = ("examples" ->
    examples.examples.distinct.sorted)

  override def unionMerge(
      otherProp: NumExamplesProperty
  )(implicit p: JsonoidParams): NumExamplesProperty = {
    NumExamplesProperty(examples.merge(otherProp.examples))
  }

  override def mergeValue(
      value: BigDecimal
  )(implicit p: JsonoidParams): NumExamplesProperty = {
    NumExamplesProperty(examples.merge(ExamplesProperty(value)))
  }
}

final case class NumMultipleOfProperty(multiple: Option[BigDecimal] = None)
    extends SchemaProperty[BigDecimal] {
  override type S = NumMultipleOfProperty

  override def newDefault()(implicit p: JsonoidParams): NumMultipleOfProperty =
    NumMultipleOfProperty()

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def toJson()(implicit p: JsonoidParams): JObject = multiple match {
    case Some(numVal) if numVal > 0 => ("multipleOf" -> numVal)
    case _                          => Nil
  }

  override def intersectMerge(
      otherProp: NumMultipleOfProperty
  )(implicit p: JsonoidParams): NumMultipleOfProperty = {
    val newMultiple = (multiple, otherProp.multiple) match {
      case (Some(m), None)    => Some(m)
      case (None, Some(n))    => Some(n)
      case (Some(m), Some(n)) => Some(lcm(m, n))
      case (None, None)       => None
    }
    NumMultipleOfProperty(newMultiple)
  }

  override def unionMerge(
      otherProp: NumMultipleOfProperty
  )(implicit p: JsonoidParams): NumMultipleOfProperty = {
    val newMultiple = (multiple, otherProp.multiple) match {
      case (Some(m), None) => Some(m)
      case (None, Some(n)) => Some(n)
      case (Some(m), Some(n)) =>
        if (m.abs < 1e-10 || n.abs < 1e-10) {
          // This avoids divide by zero errors when calculating the GCD.
          // Any multiple of values this small is unlikely to be useful anyway.
          None
        } else {
          Try(gcd(m, n)).toOption
        }
      case (None, None) => None
    }
    NumMultipleOfProperty(newMultiple)
  }

  override def mergeValue(
      value: BigDecimal
  )(implicit p: JsonoidParams): NumMultipleOfProperty = {
    unionMerge(NumMultipleOfProperty(Some(value)))
  }

  @SuppressWarnings(
    Array("org.wartremover.warts.Equals", "org.wartremover.warts.OptionPartial")
  )
  override def isCompatibleWith(
      other: NumMultipleOfProperty,
      recursive: Boolean = true
  )(implicit p: JsonoidParams): Boolean = {
    if (multiple.isEmpty) {
      // If we have no multiple, then compatible
      true
    } else if (other.multiple.isEmpty) {
      // If we have a multiple and the other schema doesn't, not compatible
      false
    } else {
      // Otherwise, the multiple must be a multiple of our multiple
      (other.multiple.get / multiple.get).isValidInt
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def expandTo(
      other: Option[NumMultipleOfProperty]
  ): NumMultipleOfProperty = {
    // Assume we have no multiple if the other prop is not specified
    (multiple, other.map(_.multiple).getOrElse(None)) match {
      case (Some(mult), Some(otherMult)) =>
        // Try halving the multiple
        val newMult = (1 to MaxExpandRounds)
          .scanLeft(mult) { (oldMult: BigDecimal, _) =>
            oldMult / 2
          }
          .find(otherMult % _ == 0)

        NumMultipleOfProperty(newMult)
      case (_, None) => NumMultipleOfProperty(None)
      case (None, _) => this
    }
  }
}

/** Tracks a histogram of number values.
  *
  * @constructor Create a new number histogram property
  * @param histogram a histogram of number values
  */
final case class NumHistogramProperty(
    histogram: Histogram = Histogram()
) extends SchemaProperty[BigDecimal] {
  override type S = NumHistogramProperty

  override def newDefault()(implicit p: JsonoidParams): NumHistogramProperty =
    NumHistogramProperty()

  override val isInformational = true

  override def toJson()(implicit p: JsonoidParams): JObject = {
    ("histogram" -> histogram.bins().map { case (value, count) =>
      List(value, count)
    })
  }

  override def unionMerge(
      otherProp: NumHistogramProperty
  )(implicit p: JsonoidParams): NumHistogramProperty = {
    NumHistogramProperty(histogram.merge(otherProp.histogram))
  }

  override def mergeValue(
      value: BigDecimal
  )(implicit p: JsonoidParams): NumHistogramProperty = {
    NumHistogramProperty(histogram.merge(value.doubleValue))
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      p: JsonoidParams,
      tag: ClassTag[S]
  ): Seq[Anomaly] = {
    val histAnomaly = value match {
      case JDouble(num)  => histogram.isAnomalous(num)
      case JDecimal(num) => histogram.isAnomalous(num.doubleValue)
      case JInt(num)     => histogram.isAnomalous(num.toDouble)
      case _             => false
    }

    if (histAnomaly) {
      Seq(Anomaly(path, "value outside histogram bounds", AnomalyLevel.Info))
    } else {
      Seq.empty
    }
  }
}

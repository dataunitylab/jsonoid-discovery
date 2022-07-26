package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import java.io.{ByteArrayOutputStream, ObjectOutputStream}
import java.util.Base64

import scala.reflect._

import com.sangupta.bloomfilter.impl.RoaringBloomFilter
import scalaz._
import org.json4s.JsonDSL._
import org.json4s._
import Scalaz._

import Helpers._
import utils.{Histogram, IntHyperLogLog}

object IntegerSchema {
  def apply(value: BigInt)(implicit propSet: PropertySet): IntegerSchema = {
    IntegerSchema(
      propSet.integerProperties.mergeValue(value)(
        EquivalenceRelations.KindEquivalenceRelation
      )
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
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ IntegerSchema(otherProperties) =>
      IntegerSchema(properties.merge(otherProperties, mergeType))
    case other: NumberSchema => other.mergeSameType(mergeType)(er)(this)
  }

  override def copy(properties: SchemaProperties[BigInt]): IntegerSchema =
    IntegerSchema(properties)
}

final case class MinIntValueProperty(
    minIntValue: Option[BigInt] = None,
    exclusive: Boolean = false
) extends SchemaProperty[BigInt, MinIntValueProperty] {
  override def toJson: JObject = ((if (exclusive) { "exclusiveMinimum" }
                                   else { "minimum" }) -> minIntValue)

  override def intersectMerge(
      otherProp: MinIntValueProperty
  )(implicit er: EquivalenceRelation): MinIntValueProperty = {
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
  )(implicit er: EquivalenceRelation): MinIntValueProperty = {
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
  )(implicit er: EquivalenceRelation): MinIntValueProperty = {
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
}

final case class MaxIntValueProperty(
    maxIntValue: Option[BigInt] = None,
    exclusive: Boolean = false
) extends SchemaProperty[BigInt, MaxIntValueProperty] {
  override def toJson: JObject = ((if (exclusive) { "exclusiveMaximum" }
                                   else { "maximum" }) -> maxIntValue)

  override def intersectMerge(
      otherProp: MaxIntValueProperty
  )(implicit er: EquivalenceRelation): MaxIntValueProperty = {
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
  )(implicit er: EquivalenceRelation): MaxIntValueProperty = {
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
  )(implicit er: EquivalenceRelation): MaxIntValueProperty = {
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
}

final case class IntHyperLogLogProperty(
    hll: IntHyperLogLog = new IntHyperLogLog()
) extends SchemaProperty[BigInt, IntHyperLogLogProperty] {
  override def toJson: JObject =
    ("distinctValues" -> hll.count()) ~ ("hll" -> hll.toBase64)

  override def unionMerge(
      otherProp: IntHyperLogLogProperty
  )(implicit er: EquivalenceRelation): IntHyperLogLogProperty = {
    val prop = IntHyperLogLogProperty()
    prop.hll.merge(this.hll)
    prop.hll.merge(otherProp.hll)

    prop
  }

  override def mergeValue(
      value: BigInt
  )(implicit er: EquivalenceRelation): IntHyperLogLogProperty = {
    val prop = IntHyperLogLogProperty()
    prop.hll.merge(this.hll)
    prop.hll.add(value.toLong)

    prop
  }
}

object IntBloomFilterProperty {
  val ExpectedElements: Int = 100000
  val FalsePositive: Double = 0.01
}

final case class IntBloomFilterProperty(
    bloomFilter: RoaringBloomFilter[Integer] = new RoaringBloomFilter[Integer](
      IntBloomFilterProperty.ExpectedElements,
      IntBloomFilterProperty.FalsePositive
    )
) extends SchemaProperty[BigInt, IntBloomFilterProperty] {
  override def toJson: JObject = {
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(bloomFilter)
    oos.close()

    val bloomStr = Base64.getEncoder().encodeToString(baos.toByteArray())
    ("bloomFilter" -> bloomStr)
  }

  override def unionMerge(
      otherProp: IntBloomFilterProperty
  )(implicit er: EquivalenceRelation): IntBloomFilterProperty = {
    val prop = IntBloomFilterProperty()
    prop.bloomFilter.merge(this.bloomFilter)
    prop.bloomFilter.merge(otherProp.bloomFilter)

    prop
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def mergeValue(
      value: BigInt
  )(implicit er: EquivalenceRelation): IntBloomFilterProperty = {
    val prop = IntBloomFilterProperty()
    prop.bloomFilter.merge(this.bloomFilter)
    prop.bloomFilter.add(value.toByteArray)

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
    extends SchemaProperty[BigInt, IntStatsProperty] {
  override def toJson: JObject = ("statistics" -> stats.toJson)

  override def unionMerge(
      otherProp: IntStatsProperty
  )(implicit er: EquivalenceRelation): IntStatsProperty = {
    IntStatsProperty(stats.merge(otherProp.stats))
  }

  override def mergeValue(
      value: BigInt
  )(implicit er: EquivalenceRelation): IntStatsProperty = {
    IntStatsProperty(stats.merge(StatsProperty(BigDecimal(value))))
  }
}

final case class IntExamplesProperty(
    examples: ExamplesProperty[BigInt] = ExamplesProperty()
) extends SchemaProperty[BigInt, IntExamplesProperty] {
  override def toJson: JObject = ("examples" ->
    examples.examples.distinct.sorted)

  override def unionMerge(
      otherProp: IntExamplesProperty
  )(implicit er: EquivalenceRelation): IntExamplesProperty = {
    IntExamplesProperty(examples.merge(otherProp.examples))
  }

  override def mergeValue(
      value: BigInt
  )(implicit er: EquivalenceRelation): IntExamplesProperty = {
    IntExamplesProperty(examples.merge(ExamplesProperty(value)))
  }
}

final case class IntMultipleOfProperty(multiple: Option[BigInt] = None)
    extends SchemaProperty[BigInt, IntMultipleOfProperty] {
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def toJson: JObject = multiple match {
    case Some(intVal) if intVal > 1 => ("multipleOf" -> intVal)
    case _                          => Nil
  }

  override def intersectMerge(
      otherProp: IntMultipleOfProperty
  )(implicit er: EquivalenceRelation): IntMultipleOfProperty = {
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
  )(implicit er: EquivalenceRelation): IntMultipleOfProperty = {
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
  )(implicit er: EquivalenceRelation): IntMultipleOfProperty = {
    unionMerge(IntMultipleOfProperty(Some(value)))
  }
}

final case class IntHistogramProperty(
    histogram: Histogram = Histogram()
) extends SchemaProperty[BigInt, IntHistogramProperty] {
  override def toJson: JObject = {
    ("histogram" -> histogram.bins.map { case (value, count) =>
      List(value.doubleValue, count.longValue)
    })
  }

  override def unionMerge(
      otherProp: IntHistogramProperty
  )(implicit er: EquivalenceRelation): IntHistogramProperty = {
    IntHistogramProperty(histogram.merge(otherProp.histogram))
  }

  override def mergeValue(
      value: BigInt
  )(implicit er: EquivalenceRelation): IntHistogramProperty = {
    IntHistogramProperty(
      histogram.merge(Histogram(List((BigDecimal(value), 1))))
    )
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      tag: ClassTag[S]
  ) = {
    value match {
      case JInt(num) =>
        if (histogram.isAnomalous(BigDecimal(num))) {
          Seq(Anomaly(path, "value outside histogram bounds", Warning))
        } else {
          Seq.empty
        }
      case _ => Seq.empty
    }
  }
}

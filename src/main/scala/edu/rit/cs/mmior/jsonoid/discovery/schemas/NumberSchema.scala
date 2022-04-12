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
import utils.{Histogram, HyperLogLog}

object NumberSchema {
  def apply(value: BigDecimal)(implicit propSet: PropertySet): NumberSchema = {
    NumberSchema(
      propSet.numberProperties.mergeValue(value)(
        EquivalenceRelations.KindEquivalenceRelation
      )
    )
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

final case class NumberSchema(
    override val properties: SchemaProperties[BigDecimal] =
      NumberSchema.AllProperties
) extends JsonSchema[BigDecimal] {
  override val schemaType = "number"

  override val validTypes: Set[Class[_]] =
    Set(classOf[JInt], classOf[JDouble], classOf[JDecimal])

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  override def mergeSameType(mergeType: MergeType)(implicit
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ NumberSchema(otherProperties) =>
      NumberSchema(properties.merge(otherProperties, mergeType))

    case other @ IntegerSchema(otherProperties) => {
      val props = SchemaProperties.empty[BigDecimal]
      otherProperties.foreach { prop =>
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
          case IntBloomFilterProperty(bloomfilter) =>
            props.add(
              NumBloomFilterProperty(
                bloomfilter.asInstanceOf[RoaringBloomFilter[Double]]
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
          case IntHistogramProperty(hist) =>
            props.add(NumHistogramProperty(hist))
          case IntMultipleOfProperty(multiple) =>
            NumMultipleOfProperty(multiple.map(_.toDouble))
        }
      }

      NumberSchema(properties.merge(props, mergeType))
    }
  }

  override def copy(properties: SchemaProperties[BigDecimal]): NumberSchema =
    NumberSchema(properties)
}

final case class MinNumValueProperty(
    minNumValue: Option[BigDecimal] = None,
    exclusive: Boolean = false
) extends SchemaProperty[BigDecimal, MinNumValueProperty] {
  override def toJson: JObject = ((if (exclusive) { "exclusiveMinimum" }
                                   else { "minimum" }) -> minNumValue)

  override def intersectMerge(
      otherProp: MinNumValueProperty
  )(implicit er: EquivalenceRelation): MinNumValueProperty = {
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
  )(implicit er: EquivalenceRelation): MinNumValueProperty = {
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
  )(implicit er: EquivalenceRelation): MinNumValueProperty = {
    val exclusive =
      this.exclusive && (minNumValue.isEmpty || value < minNumValue.get)
    MinNumValueProperty(minOrNone(Some(value), minNumValue))
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      tag: ClassTag[S]
  ) = {
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
            Seq(Anomaly(path, "value is equal or below minimum", Warning))
          case Some(true) if !exclusive =>
            Seq(Anomaly(path, "value is below minimum", Warning))
          case _ => Seq.empty
        }
      case None => Seq.empty
    }
  }
}

final case class MaxNumValueProperty(
    maxNumValue: Option[BigDecimal] = None,
    exclusive: Boolean = false
) extends SchemaProperty[BigDecimal, MaxNumValueProperty] {
  override def toJson: JObject = ((if (exclusive) { "exclusiveMaximum" }
                                   else { "maximum" }) -> maxNumValue)

  override def intersectMerge(
      otherProp: MaxNumValueProperty
  )(implicit er: EquivalenceRelation): MaxNumValueProperty = {
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
  )(implicit er: EquivalenceRelation): MaxNumValueProperty = {
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
  )(implicit er: EquivalenceRelation): MaxNumValueProperty = {
    val exclusive =
      this.exclusive && (maxNumValue.isEmpty || value < maxNumValue.get)
    MaxNumValueProperty(maxOrNone(Some(value), maxNumValue), exclusive)
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      tag: ClassTag[S]
  ) = {
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
            Seq(Anomaly(path, "value is equal or above maximum", Warning))
          case Some(true) if !exclusive =>
            Seq(Anomaly(path, "value is above maximum", Warning))
          case _ => Seq.empty
        }
      case None => Seq.empty
    }
  }
}

final case class NumHyperLogLogProperty(
    hll: HyperLogLog = new HyperLogLog()
) extends SchemaProperty[BigDecimal, NumHyperLogLogProperty] {
  override def toJson: JObject = ("distinctValues" -> hll.count()) ~ ("hll" ->
    hll.toBase64)

  override def unionMerge(
      otherProp: NumHyperLogLogProperty
  )(implicit er: EquivalenceRelation): NumHyperLogLogProperty = {
    val prop = NumHyperLogLogProperty()
    prop.hll.merge(this.hll)
    prop.hll.merge(otherProp.hll)

    prop
  }

  override def mergeValue(
      value: BigDecimal
  )(implicit er: EquivalenceRelation): NumHyperLogLogProperty = {
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

object NumBloomFilterProperty {
  val ExpectedElements: Int = 100000
  val FalsePositive: Double = 0.01
}

final case class NumBloomFilterProperty(
    bloomFilter: RoaringBloomFilter[Double] = new RoaringBloomFilter[Double](
      NumBloomFilterProperty.ExpectedElements,
      NumBloomFilterProperty.FalsePositive
    )
) extends SchemaProperty[BigDecimal, NumBloomFilterProperty] {
  override def toJson: JObject = {
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(bloomFilter)
    oos.close()

    val bloomStr = Base64.getEncoder().encodeToString(baos.toByteArray())
    ("bloomFilter" -> bloomStr)
  }

  override def unionMerge(
      otherProp: NumBloomFilterProperty
  )(implicit er: EquivalenceRelation): NumBloomFilterProperty = {
    val prop = NumBloomFilterProperty()
    prop.bloomFilter.merge(this.bloomFilter)
    prop.bloomFilter.merge(otherProp.bloomFilter)

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
  )(implicit er: EquivalenceRelation): NumBloomFilterProperty = {
    val prop = NumBloomFilterProperty()
    prop.bloomFilter.merge(this.bloomFilter)

    val scaled = scaleValue(value)
    prop.bloomFilter.add(scaled)

    prop
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      tag: ClassTag[S]
  ) = {
    val inFilter = value match {
      case JDouble(num) =>
        Some(bloomFilter.contains(scaleValue(BigDecimal(num))))
      case JDecimal(num) => Some(bloomFilter.contains(scaleValue(num)))
      case JInt(num)     => Some(bloomFilter.contains(num.toByteArray))
      case _             => None
    }

    inFilter match {
      case Some(false) =>
        Seq(Anomaly(path, "value not found in Bloom filter", Info))
      case _ => Seq.empty
    }
  }
}

final case class NumStatsProperty(stats: StatsProperty = StatsProperty())
    extends SchemaProperty[BigDecimal, NumStatsProperty] {
  override def toJson: JObject = ("statistics" -> stats.toJson)

  override def unionMerge(
      otherProp: NumStatsProperty
  )(implicit er: EquivalenceRelation): NumStatsProperty = {
    NumStatsProperty(stats.merge(otherProp.stats))
  }

  override def mergeValue(
      value: BigDecimal
  )(implicit er: EquivalenceRelation): NumStatsProperty = {
    NumStatsProperty(stats.merge(StatsProperty(value)))
  }
}

final case class NumExamplesProperty(
    examples: ExamplesProperty[BigDecimal] = ExamplesProperty()
) extends SchemaProperty[BigDecimal, NumExamplesProperty] {
  override def toJson: JObject = ("examples" ->
    examples.examples.distinct.sorted)

  override def unionMerge(
      otherProp: NumExamplesProperty
  )(implicit er: EquivalenceRelation): NumExamplesProperty = {
    NumExamplesProperty(examples.merge(otherProp.examples))
  }

  override def mergeValue(
      value: BigDecimal
  )(implicit er: EquivalenceRelation): NumExamplesProperty = {
    NumExamplesProperty(examples.merge(ExamplesProperty(value)))
  }
}

final case class NumMultipleOfProperty(multiple: Option[BigDecimal] = None)
    extends SchemaProperty[BigDecimal, NumMultipleOfProperty] {
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def toJson: JObject = multiple match {
    case Some(numVal) if numVal > 1 => ("multipleOf" -> numVal)
    case _                          => Nil
  }

  override def intersectMerge(
      otherProp: NumMultipleOfProperty
  )(implicit er: EquivalenceRelation): NumMultipleOfProperty = {
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
  )(implicit er: EquivalenceRelation): NumMultipleOfProperty = {
    val newMultiple = (multiple, otherProp.multiple) match {
      case (Some(m), None)    => Some(m)
      case (None, Some(n))    => Some(n)
      case (Some(m), Some(n)) => Some(gcd(m, n))
      case (None, None)       => None
    }
    NumMultipleOfProperty(newMultiple)
  }

  override def mergeValue(
      value: BigDecimal
  )(implicit er: EquivalenceRelation): NumMultipleOfProperty = {
    unionMerge(NumMultipleOfProperty(Some(value)))
  }
}

final case class NumHistogramProperty(
    histogram: Histogram = Histogram()
) extends SchemaProperty[BigDecimal, NumHistogramProperty] {
  override def toJson: JObject = {
    ("histogram" -> histogram.bins.map { case (value, count) =>
      List(value.doubleValue, count.longValue)
    })
  }

  override def unionMerge(
      otherProp: NumHistogramProperty
  )(implicit er: EquivalenceRelation): NumHistogramProperty = {
    NumHistogramProperty(histogram.merge(otherProp.histogram))
  }

  override def mergeValue(
      value: BigDecimal
  )(implicit er: EquivalenceRelation): NumHistogramProperty = {
    NumHistogramProperty(histogram.merge(Histogram(List((value, 1)))))
  }

  override def collectAnomalies[S <: JValue](value: S, path: String)(implicit
      tag: ClassTag[S]
  ) = {
    val histAnomaly = value match {
      case JDouble(num)  => histogram.isAnomalous(num)
      case JDecimal(num) => histogram.isAnomalous(num)
      case JInt(num)     => histogram.isAnomalous(BigDecimal(num))
      case _             => false
    }

    if (histAnomaly) {
      Seq(Anomaly(path, "value outside histogram bounds", Warning))
    } else {
      Seq.empty
    }
  }
}

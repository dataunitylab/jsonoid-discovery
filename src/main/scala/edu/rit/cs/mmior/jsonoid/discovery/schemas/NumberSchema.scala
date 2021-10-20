package edu.rit.cs.mmior.jsonoid.discovery
package schemas

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

    props
  }
}

final case class NumberSchema(
    override val properties: SchemaProperties[BigDecimal] =
      NumberSchema.AllProperties
) extends JsonSchema[BigDecimal] {
  override val schemaType = "number"

  override val validTypes: Set[ClassTag[_ <: JValue]] =
    Set(classTag[JInt], classTag[JDouble], classTag[JDecimal])

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  def mergeSameType()(implicit
      er: EquivalenceRelation
  ): PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ NumberSchema(otherProperties) =>
      NumberSchema(properties.merge(otherProperties))

    case other @ IntegerSchema(otherProperties) => {
      val props = SchemaProperties.empty[BigDecimal]
      otherProperties.foreach { prop =>
        prop match {
          case MinIntValueProperty(minValue) =>
            props.add(MinNumValueProperty(minValue.map(_.toDouble)))
          case MaxIntValueProperty(maxValue) =>
            props.add(MaxNumValueProperty(maxValue.map(_.toDouble)))
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
          case MultipleOfProperty(_) => {}
        }
      }

      NumberSchema(properties.merge(props))
    }
  }

  override def copy(properties: SchemaProperties[BigDecimal]): NumberSchema =
    NumberSchema(properties)
}

final case class MinNumValueProperty(minNumValue: Option[BigDecimal] = None)
    extends SchemaProperty[BigDecimal, MinNumValueProperty] {
  override def toJson: JObject = ("minimum" -> minNumValue)

  override def merge(
      otherProp: MinNumValueProperty
  )(implicit er: EquivalenceRelation): MinNumValueProperty = {
    MinNumValueProperty(minOrNone(minNumValue, otherProp.minNumValue))
  }

  override def mergeValue(
      value: BigDecimal
  )(implicit er: EquivalenceRelation): MinNumValueProperty = {
    MinNumValueProperty(minOrNone(Some(value), minNumValue))
  }

  override def collectAnomalies(value: JValue, path: String) = {
    minNumValue match {
      case Some(min) =>
        val exceedsMin = value match {
          case JDouble(num)  => Some(num < min)
          case JDecimal(num) => Some(num < min)
          case JInt(num)     => Some(BigDecimal(num) < min)
          case _             => None
        }

        exceedsMin match {
          case Some(true) =>
            Seq(Anomaly(path, "value is below minimum", Warning))
          case _ => Seq.empty
        }
      case None => Seq.empty
    }
  }
}

final case class MaxNumValueProperty(maxNumValue: Option[BigDecimal] = None)
    extends SchemaProperty[BigDecimal, MaxNumValueProperty] {
  override def toJson: JObject = ("maximum" -> maxNumValue)

  override def merge(
      otherProp: MaxNumValueProperty
  )(implicit er: EquivalenceRelation): MaxNumValueProperty = {
    MaxNumValueProperty(maxOrNone(maxNumValue, otherProp.maxNumValue))
  }

  override def mergeValue(
      value: BigDecimal
  )(implicit er: EquivalenceRelation): MaxNumValueProperty = {
    MaxNumValueProperty(maxOrNone(Some(value), maxNumValue))
  }

  override def collectAnomalies(value: JValue, path: String) = {
    maxNumValue match {
      case Some(max) =>
        val exceedsMax = value match {
          case JDouble(num)  => Some(num > max)
          case JDecimal(num) => Some(num > max)
          case JInt(num)     => Some(BigDecimal(num) > max)
          case _             => None
        }

        exceedsMax match {
          case Some(true) =>
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
  override def toJson: JObject = ("distinctValues" -> hll.count())

  override def merge(
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
  override def toJson: JObject = JObject(Nil)

  override def merge(
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

  override def collectAnomalies(value: JValue, path: String) = {
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

  override def merge(
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

  override def merge(
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

final case class NumHistogramProperty(
    histogram: Histogram = Histogram()
) extends SchemaProperty[BigDecimal, NumHistogramProperty] {
  override def toJson: JObject = {
    ("histogram" -> histogram.bins.map { case (value, count) =>
      List(value.doubleValue, count.longValue)
    })
  }

  override def merge(
      otherProp: NumHistogramProperty
  )(implicit er: EquivalenceRelation): NumHistogramProperty = {
    NumHistogramProperty(histogram.merge(otherProp.histogram))
  }

  override def mergeValue(
      value: BigDecimal
  )(implicit er: EquivalenceRelation): NumHistogramProperty = {
    NumHistogramProperty(histogram.merge(Histogram(List((value, 1)))))
  }

  override def collectAnomalies(value: JValue, path: String) = {
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

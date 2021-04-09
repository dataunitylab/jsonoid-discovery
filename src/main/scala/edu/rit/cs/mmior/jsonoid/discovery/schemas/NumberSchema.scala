package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import com.sangupta.bloomfilter.impl.InMemoryBloomFilter
import scalaz._
import org.json4s.JsonDSL._
import org.json4s._
import Scalaz._

import Helpers._
import utils.HyperLogLog

object NumberSchema {
  def apply(value: BigDecimal): NumberSchema = {
    NumberSchema(NumberSchema.initialProperties.merge(value))
  }

  def initialProperties: SchemaProperties[BigDecimal] =
    SchemaProperties
      .empty[BigDecimal]
      .add(MinNumValueProperty())
      .add(MaxNumValueProperty())
      .add(NumHyperLogLogProperty())
      .add(NumBloomFilterProperty())
}

final case class NumberSchema(
    override val properties: SchemaProperties[BigDecimal] =
      NumberSchema.initialProperties
) extends JsonSchema[BigDecimal] {
  override val schemaType = "number"

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ NumberSchema(otherProperties) =>
      NumberSchema(properties.merge(otherProperties))

    case other @ IntegerSchema(otherProperties) => {
      var props = SchemaProperties.empty[BigDecimal]
      otherProperties.foreach { prop =>
        prop match {
          case MinIntValueProperty(minValue) =>
            props = props.add(MinNumValueProperty(minValue.map(_.toDouble)))
          case MaxIntValueProperty(maxValue) =>
            props = props.add(MaxNumValueProperty(maxValue.map(_.toDouble)))
          case IntHyperLogLogProperty(hll) =>
            // XXX This can give some false positives due to how
            //     decimal values are tracked, but should not be
            //     a problem unless integer values are several
            //     orders of magnitude larger
            props = props.add(NumHyperLogLogProperty(hll))
          case IntBloomFilterProperty(bloomfilter) =>
            props = props.add(
              NumBloomFilterProperty(
                bloomfilter.asInstanceOf[InMemoryBloomFilter[Double]]
              )
            )
        }
      }

      NumberSchema(properties.merge(props))
    }
  }
}

final case class MinNumValueProperty(minNumValue: Option[BigDecimal] = None)
    extends SchemaProperty[BigDecimal] {
  override def toJson: JObject = ("minimum" -> minNumValue)

  override def merge(
      otherProp: SchemaProperty[BigDecimal]
  ): MinNumValueProperty = {
    MinNumValueProperty(
      minOrNone(
        minNumValue,
        otherProp.asInstanceOf[MinNumValueProperty].minNumValue
      )
    )
  }

  override def merge(value: BigDecimal): MinNumValueProperty = {
    MinNumValueProperty(minOrNone(Some(value), minNumValue))
  }
}

final case class MaxNumValueProperty(maxNumValue: Option[BigDecimal] = None)
    extends SchemaProperty[BigDecimal] {
  override def toJson: JObject = ("maximum" -> maxNumValue)

  override def merge(
      otherProp: SchemaProperty[BigDecimal]
  ): MaxNumValueProperty = {
    MaxNumValueProperty(
      maxOrNone(
        maxNumValue,
        otherProp.asInstanceOf[MaxNumValueProperty].maxNumValue
      )
    )
  }

  override def merge(value: BigDecimal): MaxNumValueProperty = {
    MaxNumValueProperty(maxOrNone(Some(value), maxNumValue))
  }
}

final case class NumHyperLogLogProperty(
    hll: HyperLogLog = new HyperLogLog()
) extends SchemaProperty[BigDecimal] {
  override def toJson: JObject = ("distinctValues" -> hll.count())

  override def merge(
      otherProp: SchemaProperty[BigDecimal]
  ): NumHyperLogLogProperty = {
    val prop = NumHyperLogLogProperty()
    prop.hll.merge(this.hll)
    prop.hll.merge(otherProp.asInstanceOf[NumHyperLogLogProperty].hll)

    prop
  }

  override def merge(value: BigDecimal): NumHyperLogLogProperty = {
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
    bloomFilter: InMemoryBloomFilter[Double] = new InMemoryBloomFilter[Double](
      NumBloomFilterProperty.ExpectedElements,
      NumBloomFilterProperty.FalsePositive
    )
) extends SchemaProperty[BigDecimal] {
  override def toJson: JObject = JObject(Nil)

  override def merge(
      otherProp: SchemaProperty[BigDecimal]
  ): NumBloomFilterProperty = {
    val prop = NumBloomFilterProperty()
    prop.bloomFilter.merge(this.bloomFilter)
    prop.bloomFilter.merge(
      otherProp.asInstanceOf[NumBloomFilterProperty].bloomFilter
    )

    prop
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def merge(value: BigDecimal): NumBloomFilterProperty = {
    val prop = NumBloomFilterProperty()
    prop.bloomFilter.merge(this.bloomFilter)

    val scaled = value.toBigIntExact match {
      case Some(int) => int.toByteArray
      case None      => value.toString.getBytes
    }
    prop.bloomFilter.add(scaled)

    prop
  }
}

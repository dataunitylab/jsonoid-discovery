package edu.rit.cs.mmior.jsonoid.discovery
package schemas

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

  def initialProperties: SchemaProperties[BigDecimal] = SchemaProperties(
    MinNumValueProperty(),
    MaxNumValueProperty(),
    NumHyperLogLogProperty()
  )
}

final case class NumberSchema(
    override val properties: SchemaProperties[BigDecimal] =
      NumberSchema.initialProperties
) extends JsonSchema[BigDecimal] {
  override val schemaType = "number"

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ NumberSchema(otherProperties) =>
      NumberSchema(properties.merge(otherProperties))

    case other @ IntegerSchema(otherProperties) => {
      val newProperties: Seq[SchemaProperty[BigDecimal]] =
        otherProperties.collect {
          case MinIntValueProperty(minValue) =>
            MinNumValueProperty(minValue.map(_.toDouble))
          case MaxIntValueProperty(maxValue) =>
            MaxNumValueProperty(maxValue.map(_.toDouble))
          case IntHyperLogLogProperty(hll) =>
            // XXX This can give some false positives due to how
            //     decimal values are tracked, but should not be
            //     a problem unless integer values are several
            //     orders of magnitude larger
            NumHyperLogLogProperty(hll)
        }.toSeq

      NumberSchema(properties.merge(SchemaProperties(newProperties)))
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

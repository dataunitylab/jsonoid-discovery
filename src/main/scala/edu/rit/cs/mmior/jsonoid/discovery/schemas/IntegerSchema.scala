package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import com.sangupta.bloomfilter.impl.InMemoryBloomFilter
import scalaz._
import org.json4s.JsonDSL._
import org.json4s._
import Scalaz._

import Helpers._
import utils.HyperLogLog

object IntegerSchema {
  def apply(value: BigInt): IntegerSchema = {
    IntegerSchema(IntegerSchema.initialProperties.merge(value))
  }

  def initialProperties: SchemaProperties[BigInt] = SchemaProperties(
    MinIntValueProperty(),
    MaxIntValueProperty(),
    IntHyperLogLogProperty(),
    IntBloomFilterProperty()
  )
}

final case class IntegerSchema(
    override val properties: SchemaProperties[BigInt] =
      IntegerSchema.initialProperties
) extends JsonSchema[BigInt] {
  override val schemaType = "integer"

  def mergeSameType: PartialFunction[JsonSchema[_], JsonSchema[_]] = {
    case other @ IntegerSchema(otherProperties) =>
      IntegerSchema(properties.merge(otherProperties))
    case other: NumberSchema => other.mergeSameType(this)
  }
}

final case class MinIntValueProperty(minIntValue: Option[BigInt] = None)
    extends SchemaProperty[BigInt] {
  override def toJson: JObject = ("minimum" -> minIntValue)

  override def merge(otherProp: SchemaProperty[BigInt]): MinIntValueProperty = {
    MinIntValueProperty(
      minOrNone(
        minIntValue,
        otherProp.asInstanceOf[MinIntValueProperty].minIntValue
      )
    )
  }

  override def merge(value: BigInt): MinIntValueProperty = {
    MinIntValueProperty(minOrNone(Some(value), minIntValue))
  }
}

final case class MaxIntValueProperty(maxIntValue: Option[BigInt] = None)
    extends SchemaProperty[BigInt] {
  override def toJson: JObject = ("maximum" -> maxIntValue)

  override def merge(otherProp: SchemaProperty[BigInt]): MaxIntValueProperty = {
    MaxIntValueProperty(
      maxOrNone(
        maxIntValue,
        otherProp.asInstanceOf[MaxIntValueProperty].maxIntValue
      )
    )
  }

  override def merge(value: BigInt): MaxIntValueProperty = {
    MaxIntValueProperty(maxOrNone(Some(value), maxIntValue))
  }
}

final case class IntHyperLogLogProperty(
    hll: HyperLogLog = new HyperLogLog()
) extends SchemaProperty[BigInt] {
  override def toJson: JObject = ("distinctValues" -> hll.count())

  override def merge(
      otherProp: SchemaProperty[BigInt]
  ): IntHyperLogLogProperty = {
    val prop = IntHyperLogLogProperty()
    prop.hll.merge(this.hll)
    prop.hll.merge(otherProp.asInstanceOf[IntHyperLogLogProperty].hll)

    prop
  }

  override def merge(value: BigInt): IntHyperLogLogProperty = {
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
    bloomFilter: InMemoryBloomFilter[Integer] =
      new InMemoryBloomFilter[Integer](
        IntBloomFilterProperty.ExpectedElements,
        IntBloomFilterProperty.FalsePositive
      )
) extends SchemaProperty[BigInt] {
  override def toJson: JObject = JObject(Nil)

  override def merge(
      otherProp: SchemaProperty[BigInt]
  ): IntBloomFilterProperty = {
    val prop = IntBloomFilterProperty()
    prop.bloomFilter.merge(this.bloomFilter)
    prop.bloomFilter.merge(
      otherProp.asInstanceOf[IntBloomFilterProperty].bloomFilter
    )

    prop
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def merge(value: BigInt): IntBloomFilterProperty = {
    val prop = IntBloomFilterProperty()
    prop.bloomFilter.merge(this.bloomFilter)
    prop.bloomFilter.add(value.toByteArray)

    prop
  }
}

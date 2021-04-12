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
    IntegerSchema(IntegerSchema.initialProperties.mergeValue(value))
  }

  def initialProperties: SchemaProperties[BigInt] =
    SchemaProperties
      .empty[BigInt]
      .add(MinIntValueProperty())
      .add(MaxIntValueProperty())
      .add(IntHyperLogLogProperty())
      .add(IntBloomFilterProperty())
      .add(IntStatsProperty())
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
    extends SchemaProperty[BigInt, MinIntValueProperty] {
  override def toJson: JObject = ("minimum" -> minIntValue)

  override def merge(otherProp: MinIntValueProperty): MinIntValueProperty = {
    MinIntValueProperty(minOrNone(minIntValue, otherProp.minIntValue))
  }

  override def mergeValue(value: BigInt): MinIntValueProperty = {
    MinIntValueProperty(minOrNone(Some(value), minIntValue))
  }
}

final case class MaxIntValueProperty(maxIntValue: Option[BigInt] = None)
    extends SchemaProperty[BigInt, MaxIntValueProperty] {
  override def toJson: JObject = ("maximum" -> maxIntValue)

  override def merge(otherProp: MaxIntValueProperty): MaxIntValueProperty = {
    MaxIntValueProperty(maxOrNone(maxIntValue, otherProp.maxIntValue))
  }

  override def mergeValue(value: BigInt): MaxIntValueProperty = {
    MaxIntValueProperty(maxOrNone(Some(value), maxIntValue))
  }
}

final case class IntHyperLogLogProperty(
    hll: HyperLogLog = new HyperLogLog()
) extends SchemaProperty[BigInt, IntHyperLogLogProperty] {
  override def toJson: JObject = ("distinctValues" -> hll.count())

  override def merge(
      otherProp: IntHyperLogLogProperty
  ): IntHyperLogLogProperty = {
    val prop = IntHyperLogLogProperty()
    prop.hll.merge(this.hll)
    prop.hll.merge(otherProp.hll)

    prop
  }

  override def mergeValue(value: BigInt): IntHyperLogLogProperty = {
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
) extends SchemaProperty[BigInt, IntBloomFilterProperty] {
  override def toJson: JObject = JObject(Nil)

  override def merge(
      otherProp: IntBloomFilterProperty
  ): IntBloomFilterProperty = {
    val prop = IntBloomFilterProperty()
    prop.bloomFilter.merge(this.bloomFilter)
    prop.bloomFilter.merge(otherProp.bloomFilter)

    prop
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def mergeValue(value: BigInt): IntBloomFilterProperty = {
    val prop = IntBloomFilterProperty()
    prop.bloomFilter.merge(this.bloomFilter)
    prop.bloomFilter.add(value.toByteArray)

    prop
  }
}

final case class IntStatsProperty(stats: StatsProperty = StatsProperty())
    extends SchemaProperty[BigInt, IntStatsProperty] {
  override def toJson: JObject = ("stats" -> stats.toJson)

  override def merge(
      otherProp: IntStatsProperty
  ): IntStatsProperty = {
    IntStatsProperty(stats.merge(otherProp.stats))
  }

  override def mergeValue(value: BigInt): IntStatsProperty = {
    IntStatsProperty(stats.merge(StatsProperty(BigDecimal(value))))
  }
}

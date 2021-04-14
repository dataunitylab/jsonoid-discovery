package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._

import UnitSpec._

class IntegerSchemaSpec extends UnitSpec {
  behavior of "IntegerSchema"

  private val integerSchema = IntegerSchema(3).merge(IntegerSchema(4)).asInstanceOf[IntegerSchema]

  it should "track the maximum length" in {
    integerSchema.properties should contain (MaxIntValueProperty(Some(4)))
  }

  it should "track the minimum length" in {
    integerSchema.properties should contain (MinIntValueProperty(Some(3)))
  }

  it should "track the distinct elements" in {
    val hyperLogLogProp = integerSchema.properties.get[IntHyperLogLogProperty]
    hyperLogLogProp.hll.count() should be (2)
  }

  it should "keep a Bloom filter of observed elements" in {
    val bloomFilterProp = integerSchema.properties.get[IntBloomFilterProperty]
    bloomFilterProp.bloomFilter.contains(BigInt(3).toByteArray) shouldBe true
  }

  it should "keep statistics" in {
    val statsProp = integerSchema.properties.get[IntStatsProperty]
    statsProp.stats.mean shouldBe (BigDecimal(3.5))
  }

  it should "keep examples" in {
    val examplesProp = integerSchema.properties.get[IntExamplesProperty]
    (examplesProp.toJson \ "examples") shouldEqual JArray(List(3, 4))
  }
}

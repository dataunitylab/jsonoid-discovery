package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import UnitSpec._

class IntegerSchemaSpec extends UnitSpec {
  behavior of "IntegerSchema"

  private val integerSchema = IntegerSchema(3).merge(IntegerSchema(4))

  it should "track the maximum length" in {
    integerSchema.properties should contain (MaxIntValueProperty(Some(4)))
  }

  it should "track the minimum length" in {
    integerSchema.properties should contain (MinIntValueProperty(Some(3)))
  }

  it should "track the distinct elements" in {
    val hyperLogLogProp = integerSchema.properties.find(_.isInstanceOf[IntHyperLogLogProperty]).fold(IntHyperLogLogProperty())(_.asInstanceOf[IntHyperLogLogProperty])
    hyperLogLogProp.hll.count() should be (2)
  }

  it should "keep a Bloom filter of observed elements" in {
    val bloomFilterProp = integerSchema.properties.find(_.isInstanceOf[IntBloomFilterProperty]).fold(IntBloomFilterProperty())(_.asInstanceOf[IntBloomFilterProperty])
    bloomFilterProp.bloomFilter.contains(BigInt(3).toByteArray) shouldBe true
  }
}
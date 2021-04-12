package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import UnitSpec._

class NumberSchemaSpec extends UnitSpec {
  behavior of "NumberSchema"

  private val numberSchema = NumberSchema(3.14).merge(NumberSchema(4.28))
  private val integerSchema = IntegerSchema(5)
  private val mixedSchema = numberSchema.merge(integerSchema)

  it should "track the maximum value" in {
    numberSchema.properties should contain (MaxNumValueProperty(Some(4.28)))
  }

  it should "track the minimum value" in {
    numberSchema.properties should contain (MinNumValueProperty(Some(3.14)))
  }

  it should "track the distinct elements" in {
    val hyperLogLogProp = numberSchema.properties.find(_.isInstanceOf[NumHyperLogLogProperty]).fold(NumHyperLogLogProperty())(_.asInstanceOf[NumHyperLogLogProperty])
    hyperLogLogProp.hll.count() should be (2)
  }

  it should "keep a Bloom filter of observed elements" in {
    val bloomFilterProp = numberSchema.properties.find(_.isInstanceOf[NumBloomFilterProperty]).fold(NumBloomFilterProperty())(_.asInstanceOf[NumBloomFilterProperty])
    bloomFilterProp.bloomFilter.contains(BigDecimal(3.14).toString.getBytes) shouldBe true
  }

  it should "keep statistics" in {
    val statsProp = numberSchema.properties.find(_.isInstanceOf[NumStatsProperty]).fold(NumStatsProperty())(_.asInstanceOf[NumStatsProperty])
    statsProp.stats.mean shouldBe (BigDecimal(3.71))
  }

  it should "track the maximum value when merged with an integer schema" in {
    mixedSchema.properties should contain (MaxNumValueProperty(Some(5)))
  }

  it should "track the minimum value when merged with an integer schema" in {
    mixedSchema.properties should contain (MinNumValueProperty(Some(3.14)))
  }

  it should "track the distinct elements when merged with an integer schema" in {
    val hyperLogLogProp = mixedSchema.properties.find(_.isInstanceOf[NumHyperLogLogProperty]).fold(NumHyperLogLogProperty())(_.asInstanceOf[NumHyperLogLogProperty])
    hyperLogLogProp.hll.count() should be (3)
  }

  it should "keep a Bloom filter of observed elements when merged with an integer schema" in {
    val bloomFilterProp = mixedSchema.properties.find(_.isInstanceOf[NumBloomFilterProperty]).fold(NumBloomFilterProperty())(_.asInstanceOf[NumBloomFilterProperty])
    bloomFilterProp.bloomFilter.contains(BigInt(5).toByteArray) shouldBe true
  }

  it should "keep statistics when merged with an integer schema" in {
    val statsProp = mixedSchema.properties.find(_.isInstanceOf[NumStatsProperty]).fold(NumStatsProperty())(_.asInstanceOf[NumStatsProperty])
    statsProp.stats.mean shouldBe (BigDecimal(4.14))
  }
}

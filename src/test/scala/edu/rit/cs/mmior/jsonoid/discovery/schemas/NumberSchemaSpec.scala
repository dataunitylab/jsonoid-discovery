package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._

import PropertySets._
import UnitSpec._

class NumberSchemaSpec extends UnitSpec {
  behavior of "NumberSchema"

  private val numberSchema = NumberSchema(3.14).merge(NumberSchema(4.28)).asInstanceOf[NumberSchema]
  private val integerSchema = IntegerSchema(5)
  private val mixedSchema = numberSchema.merge(integerSchema).asInstanceOf[NumberSchema]

  it should "track the maximum value" in {
    numberSchema.properties should contain (MaxNumValueProperty(Some(4.28)))
  }

  it should "track the minimum value" in {
    numberSchema.properties should contain (MinNumValueProperty(Some(3.14)))
  }

  it should "track the distinct elements" in {
    val hyperLogLogProp = numberSchema.properties.get[NumHyperLogLogProperty]
    hyperLogLogProp.hll.count() should be (2)
  }

  it should "keep a Bloom filter of observed elements" in {
    val bloomFilterProp = numberSchema.properties.get[NumBloomFilterProperty]
    bloomFilterProp.bloomFilter.contains(BigDecimal(3.14).toString.getBytes) shouldBe true
  }

  it should "keep statistics" in {
    val statsProp = numberSchema.properties.get[NumStatsProperty]
    statsProp.stats.mean shouldBe (BigDecimal(3.71))
  }

  it should "keep examples" in {
    val examplesProp = numberSchema.properties.get[NumExamplesProperty]
    (examplesProp.toJson \ "examples") shouldEqual JArray(List(BigDecimal(3.14), BigDecimal(4.28)))
  }

  it should "keep a running histogram" in {
    val histProp = numberSchema.properties.get[NumHistogramProperty]
    histProp.histogram.bins shouldBe List((3.14, 1), (4.28, 1))
  }

  it should "track the maximum value when merged with an integer schema" in {
    mixedSchema.properties should contain (MaxNumValueProperty(Some(5)))
  }

  it should "track the minimum value when merged with an integer schema" in {
    mixedSchema.properties should contain (MinNumValueProperty(Some(3.14)))
  }

  it should "track the distinct elements when merged with an integer schema" in {
    val hyperLogLogProp = mixedSchema.properties.get[NumHyperLogLogProperty]
    hyperLogLogProp.hll.count() should be (3)
  }

  it should "keep a Bloom filter of observed elements when merged with an integer schema" in {
    val bloomFilterProp = mixedSchema.properties.get[NumBloomFilterProperty]
    bloomFilterProp.bloomFilter.contains(BigInt(5).toByteArray) shouldBe true
  }

  it should "keep statistics when merged with an integer schema" in {
    val statsProp = mixedSchema.properties.get[NumStatsProperty]
    statsProp.stats.mean shouldBe (BigDecimal(4.14))
  }

  it should "keep examples when merged with an integer schema" in {
    val examplesProp = mixedSchema.properties.get[NumExamplesProperty]
    (examplesProp.toJson \ "examples") shouldEqual JArray(List(BigDecimal(3.14), BigDecimal(4.28), BigDecimal(5)))
  }

  it should "keep a running histogram when merged with an integer schema" in {
    val histProp = mixedSchema.properties.get[NumHistogramProperty]
    histProp.histogram.bins shouldBe List((3.14, 1), (4.28, 1), (5, 1))
  }
}

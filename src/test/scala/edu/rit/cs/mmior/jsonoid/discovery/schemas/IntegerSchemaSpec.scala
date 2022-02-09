package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._

import PropertySets._
import UnitSpec._

class IntegerSchemaSpec extends UnitSpec {
  behavior of "IntegerSchema"

  private val integerSchema =
    IntegerSchema(8).merge(IntegerSchema(4)).asInstanceOf[IntegerSchema]

  it should "track the maximum length" in {
    integerSchema.properties should contain(MaxIntValueProperty(Some(8)))
  }

  it should "track the minimum length" in {
    integerSchema.properties should contain(MinIntValueProperty(Some(4)))
  }

  it should "track the distinct elements" in {
    val hyperLogLogProp = integerSchema.properties.get[IntHyperLogLogProperty]
    hyperLogLogProp.hll.count() should be(2)
  }

  it should "keep a Bloom filter of observed elements" in {
    val bloomFilterProp = integerSchema.properties.get[IntBloomFilterProperty]
    bloomFilterProp.bloomFilter.contains(BigInt(8).toByteArray) shouldBe true
  }

  it should "keep statistics" in {
    val statsProp = integerSchema.properties.get[IntStatsProperty]
    statsProp.stats.mean shouldBe (BigDecimal(6))
  }

  it should "keep examples" in {
    val examplesProp = integerSchema.properties.get[IntExamplesProperty]
    (examplesProp.toJson \ "examples") shouldEqual JArray(List(4, 8))
  }

  it should "track a common multiple" in {
    val multipleProp = integerSchema.properties.get[IntMultipleOfProperty]
    multipleProp.multiple.value shouldBe (4)
  }

  it should "not track multiples of zero" in {
    val zeroIntSchema =
      IntegerSchema(0).merge(IntegerSchema(0)).asInstanceOf[IntegerSchema]
    val multipleProp = zeroIntSchema.properties.get[IntMultipleOfProperty]
    multipleProp.toJson shouldBe JObject()
  }

  it should "keep a running histogram" in {
    val histProp = integerSchema.properties.get[IntHistogramProperty]
    histProp.histogram.bins shouldBe List((4, 1), (8, 1))
  }

  it should "have no properties in the minimal property set" in {
    IntegerSchema(0)(PropertySets.MinProperties).properties shouldBe empty
  }

  it should "show integers as a valid type" in {
    integerSchema.isValidType(JInt(3)) shouldBe (true)
  }

  it should "show numbers as an invalid type" in {
    integerSchema.isValidType(JDouble(3.4)) shouldBe (false)
  }

  it should "not detect anomalies when an integer is in range" in {
    integerSchema.collectAnomalies(JInt(4)) shouldBe empty
  }

  it should "not detect anomalies for non-integer values" in {
    integerSchema.properties.flatMap(
      _.collectAnomalies(JDouble(3.4))
    ) shouldBe empty
  }

  it should "detect anomalies when a value is too small" in {
    integerSchema.properties
      .get[MinIntValueProperty]
      .collectAnomalies(JInt(3)) shouldBe Seq(
      Anomaly("$", "value is below minimum", Warning)
    )
  }

  it should "detect anomalies when a value is too large" in {
    integerSchema.properties
      .get[MaxIntValueProperty]
      .collectAnomalies(JInt(50)) shouldBe Seq(
      Anomaly("$", "value is above maximum", Warning)
    )
  }

  it should "detect anomalies when a value has not been observed" in {
    integerSchema.properties
      .get[IntBloomFilterProperty]
      .collectAnomalies(JInt(2)) shouldBe Seq(
      Anomaly("$", "value not found in Bloom filter", Info)
    )
  }

  it should "detect anomalies when a value outside of the histogram range" in {
    integerSchema.properties
      .get[IntHistogramProperty]
      .collectAnomalies(JInt(30)) shouldBe Seq(
      Anomaly("$", "value outside histogram bounds", Warning)
    )
  }
}

package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._

import PropertySets._
import UnitSpec._

class IntegerSchemaSpec extends UnitSpec {
  implicit val formats: Formats = DefaultFormats

  private val integerSchema =
    IntegerSchema(8).merge(IntegerSchema(4)).asInstanceOf[IntegerSchema]
  private val integerSchemaIntersect =
    IntegerSchema(8)
      .merge(IntegerSchema(4), Intersect)
      .asInstanceOf[IntegerSchema]

  behavior of "MaxIntValueProperty"

  it should "track the maximum value" in {
    integerSchema.properties should contain(MaxIntValueProperty(Some(8)))
  }

  it should "track the maximum value on intersection" in {
    integerSchemaIntersect.properties should contain(
      MaxIntValueProperty(Some(4))
    )
  }

  behavior of "MinIntValueProperty"

  it should "track the minimum value" in {
    integerSchema.properties should contain(MinIntValueProperty(Some(4)))
  }

  it should "track the minimum value on intersection" in {
    integerSchemaIntersect.properties should contain(
      MinIntValueProperty(Some(8))
    )
  }

  behavior of "IntHyperLogLogProperty"

  it should "track the distinct elements" in {
    val hyperLogLogProp = integerSchema.properties.get[IntHyperLogLogProperty]
    hyperLogLogProp.hll.count() should be(2)
  }

  behavior of "IntBloomFilterProperty"

  it should "keep a Bloom filter of observed elements" in {
    val bloomFilterProp = integerSchema.properties.get[IntBloomFilterProperty]
    bloomFilterProp.bloomFilter.contains(BigInt(8).toByteArray) shouldBe true
  }

  behavior of "IntStatsProperty"

  it should "keep statistics" in {
    val statsProp = integerSchema.properties.get[IntStatsProperty]
    statsProp.stats.mean shouldBe (BigDecimal(6))
  }

  behavior of "IntExamplesProperty"

  it should "keep examples" in {
    val examplesProp = integerSchema.properties.get[IntExamplesProperty]
    (examplesProp.toJson \ "examples") shouldEqual JArray(List(4, 8))
  }

  behavior of "IntMultipleOfProperty"

  it should "track a common multiple" in {
    val multipleProp = integerSchema.properties.get[IntMultipleOfProperty]
    multipleProp.multiple.value shouldBe (4)
  }

  it should "track a common multiple on intersect" in {
    val multipleProp =
      integerSchemaIntersect.properties.get[IntMultipleOfProperty]
    multipleProp.multiple.value shouldBe (8)
  }

  it should "not track multiples of zero" in {
    val zeroIntSchema =
      IntegerSchema(0).merge(IntegerSchema(0)).asInstanceOf[IntegerSchema]
    val multipleProp = zeroIntSchema.properties.get[IntMultipleOfProperty]
    multipleProp.toJson shouldBe JObject()
  }

  behavior of "IntHistogramProperty"

  it should "keep a running histogram" in {
    val histProp = integerSchema.properties.get[IntHistogramProperty]
    val bins = (histProp.toJson \ "histogram").extract[List[List[Double]]]
    bins(0)(0) should equal(4.0 +- 0.1)
    bins(0)(1) should ===(1.0)
    bins(1)(0) should ===(8.0 +- 0.1)
    bins(1)(1) should ===(1.0)
  }

  behavior of "IntegerSchema"

  it should "have no properties in the minimal property set" in {
    IntegerSchema(0)(
      PropertySets.MinProperties,
      JsonoidParams.defaultJsonoidParams
    ).properties shouldBe empty
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

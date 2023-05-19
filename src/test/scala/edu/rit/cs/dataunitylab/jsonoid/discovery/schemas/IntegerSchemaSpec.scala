package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._

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

  it should "expand by incrementing values" in {
    MaxIntValueProperty(Some(10))
      .expandTo(Some(MaxIntValueProperty((Some(11)))))
      .maxIntValue shouldBe Some(11)
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

  it should "expand by decrementing values" in {
    MinIntValueProperty(Some(14))
      .expandTo(Some(MinIntValueProperty((Some(13)))))
      .minIntValue shouldBe Some(13)
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
    (examplesProp.toJson() \ "examples") shouldEqual JArray(List(4, 8))
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
    multipleProp.toJson() shouldBe JObject()
  }

  it should "be compatible with the same multiple" in {
    IntMultipleOfProperty(Some(3)).isSubsetOf(
      IntMultipleOfProperty(Some(3))
    ) shouldBe true
  }

  it should "be compatible with a larger multiple" in {
    IntMultipleOfProperty(Some(6)).isSubsetOf(
      IntMultipleOfProperty(Some(3))
    ) shouldBe true
  }

  it should "not be compatible with a smaller multiple" in {
    IntMultipleOfProperty(Some(2)).isSubsetOf(
      IntMultipleOfProperty(Some(4))
    ) shouldBe false
  }

  it should "be compatible if no multiple" in {
    IntMultipleOfProperty(Some(2)).isSubsetOf(
      IntMultipleOfProperty(None)
    ) shouldBe true
  }

  it should "not be compatible with a zero multiple" in {
    IntMultipleOfProperty(Some(14)).isSubsetOf(
      IntMultipleOfProperty(Some(0))
    ) shouldBe false
  }

  it should "expand to remove a prime factor" in {
    IntMultipleOfProperty(Some(2 * 3 * 5 * 7))
      .expandTo(Some(IntMultipleOfProperty((Some(5 * 7)))))
      .multiple shouldBe Some(5 * 7)
  }

  it should "stop expanding after MaxExpandRounds" in {
    IntMultipleOfProperty(Some(1048576))
      .expandTo(Some(IntMultipleOfProperty((Some(2)))))
      .multiple shouldBe None
  }

  it should "not expand if already covered" in {
    IntMultipleOfProperty(Some(14))
      .expandTo(Some(IntMultipleOfProperty((Some(28)))))
      .multiple shouldBe Some(14)
  }

  behavior of "IntHistogramProperty"

  it should "keep a running histogram" in {
    val histProp = integerSchema.properties.get[IntHistogramProperty]
    val histJson = histProp.toJson() \ "histogram"
    val bins = (histJson \ "bins").extract[List[List[Double]]]
    bins(0)(0) should equal(4.0 +- 0.1)
    bins(0)(1) should ===(1.0)
    bins(1)(0) should ===(8.0 +- 0.1)
    bins(1)(1) should ===(1.0)
    (histJson \ "hasExtremeValues").extract[Boolean] shouldBe false
  }

  it should "report extreme values" in {
    val largeIntSchema = IntegerSchema(
      BigInt(
        "344440000000000000124000000124154004000124000000124154004000000000004154340000004340000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000021058799293176151000000000000000000000000000000000000000000000000000000000000000000000000000000210587992931761514800"
      )
    )
    val histProp = largeIntSchema.properties.get[IntHistogramProperty]
    val histJson = histProp.toJson() \ "histogram"
    (histJson \ "hasExtremeValues").extract[Boolean] shouldBe true
  }

  behavior of "IntegerSchema"

  it should "have no properties in the minimal property set" in {
    IntegerSchema(0)(
      JsonoidParams().withPropertySet(PropertySets.MinProperties)
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
      Anomaly("$", "value is below minimum", AnomalyLevel.Warning)
    )
  }

  it should "detect anomalies when a value is too large" in {
    integerSchema.properties
      .get[MaxIntValueProperty]
      .collectAnomalies(JInt(50)) shouldBe Seq(
      Anomaly("$", "value is above maximum", AnomalyLevel.Warning)
    )
  }

  it should "detect anomalies when a value has not been observed" in {
    integerSchema.properties
      .get[IntBloomFilterProperty]
      .collectAnomalies(JInt(2)) shouldBe Seq(
      Anomaly("$", "value not found in Bloom filter", AnomalyLevel.Info)
    )
  }

  it should "detect anomalies when a value outside of the histogram range" in {
    integerSchema.properties
      .get[IntHistogramProperty]
      .collectAnomalies(JInt(30)) shouldBe Seq(
      Anomaly("$", "value outside histogram bounds", AnomalyLevel.Info)
    )
  }

  it should "be compatible with a matching schema" in {
    IntegerSchema(1).isSubsetOf(IntegerSchema(1)) shouldBe true
  }

  it should "expand to be compatible with a similar schema" in {
    val schema = IntegerSchema(0)
    schema.isSubsetOf(IntegerSchema(1).expandTo(Some(schema))) shouldBe true
  }
}

package io.github.dataunitylab.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import UnitSpec._

class IntegerSchemaSpec extends UnitSpec with ScalaCheckPropertyChecks {
  implicit val formats: Formats = DefaultFormats

  private val integerSchema =
    IntegerSchema(8).merge(IntegerSchema(4)).asInstanceOf[IntegerSchema]
  private val integerSchemaIntersect =
    IntegerSchema(8)
      .merge(IntegerSchema(4), Intersect)
      .asInstanceOf[IntegerSchema]

  it should "be a subset of itself" in {
    forAll(SchemaGen.genIntSchema) { schema =>
      schema.isSubsetOf(schema).shouldBe(true)
    }
  }

  it should "round trip JSON conversion" in {
    forAll(SchemaGen.genIntSchema) { schema =>
      val convertedSchema = IntegerSchema.fromJson(schema.toJson())
      convertedSchema.isSubsetOf(schema).shouldBe(true)
      schema.isSubsetOf(convertedSchema).shouldBe(true)
    }
  }

  it should "schemas generated from a value should not be anonmalous" in {
    forAll(JsonGen.genInt) { int =>
      val schema = IntegerSchema(int.num)
      schema.isAnomalous(int) shouldBe false
    }
  }

  it should "always create merged values which are subsets" in {
    forAll(SchemaGen.genIntSchema, SchemaGen.genIntSchema) {
      case (schema1, schema2) =>
        val mergedSchema = schema1.merge(schema2).asInstanceOf[IntegerSchema]
        schema1.isSubsetOf(mergedSchema).shouldBe(true)
        schema2.isSubsetOf(mergedSchema).shouldBe(true)
    }
  }

  it should "be a subset of itself as a NumberSchema" in {
    forAll(SchemaGen.genIntSchema) { schema =>
      {
        val numberSchema = schema.asNumberSchema
        numberSchema.isSubsetOf(schema).shouldBe(true)
        schema.isSubsetOf(numberSchema).shouldBe(true)
      }
    }
  }

  it should "be a subset iff there are incompatibilities" in {
    forAll(SchemaGen.genIntSchema, SchemaGen.genIntSchema) {
      case (schema1, schema2) =>
        val incompatibilities = schema1.findIncompatibilities(schema2, true)
        schema1.isSubsetOf(schema2).shouldBe(incompatibilities.isEmpty)
    }
  }

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
      .maxIntValue
      .value shouldBe 11
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
      .minIntValue
      .value shouldBe 13
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
      .multiple
      .value shouldBe 14
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

  ignore should "report extreme values" in {
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

  it should "have no properties in the minimal property set" in withParams(
    propSet = PropertySets.MinProperties
  ) { implicit params =>
    IntegerSchema(0)(params).properties shouldBe empty
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

  behavior of "fromJson"

  it should "parse examples" in {
    val intSchema = IntegerSchema.fromJson(("examples" -> List(2)))
    val examplesProp = intSchema.properties.get[IntExamplesProperty]
    (examplesProp.toJson() \ "examples") shouldEqual JArray(List(2))
  }

  it should "reconstruct a Bloom filter" in {
    val intSchema = IntegerSchema.fromJson(
      ("bloomFilter" -> "rO0ABXNyADppby5naXRodWIuZGF0YXVuaXR5bGFiLmpzb25vaWQuZGlzY292ZXJ5LnV0aWxzLkJsb29tRmlsdGVyAyNWew3ef0cCAAFMAAZmaWx0ZXJ0ADtMaW8vZ2l0aHViL21pY2hhZWxtaW9yL2Jsb29tZmlsdGVyL2ltcGwvUm9hcmluZ0Jsb29tRmlsdGVyO3hwc3IAOWlvLmdpdGh1Yi5taWNoYWVsbWlvci5ibG9vbWZpbHRlci5pbXBsLlJvYXJpbmdCbG9vbUZpbHRlckpNNnXYMqaeAgAAeHIANWlvLmdpdGh1Yi5taWNoYWVsbWlvci5ibG9vbWZpbHRlci5BYnN0cmFjdEJsb29tRmlsdGVyyfiSaZRXHSMCAAZJABhrT3JOdW1iZXJPZkhhc2hGdW5jdGlvbnNEABttYXhGYWxzZVBvc2l0aXZlUHJvYmFiaWxpdHlJAA9udW1CaXRzUmVxdWlyZWRMAAhiaXRBcnJheXQAMUxpby9naXRodWIvbWljaGFlbG1pb3IvYmxvb21maWx0ZXIvY29yZS9CaXRBcnJheTtMABBjdXN0b21EZWNvbXBvc2VydAA4TGlvL2dpdGh1Yi9taWNoYWVsbWlvci9ibG9vbWZpbHRlci9kZWNvbXBvc2UvRGVjb21wb3NlcjtMAAZoYXNoZXJ0ADVMaW8vZ2l0aHViL21pY2hhZWxtaW9yL2Jsb29tZmlsdGVyL2hhc2gvSGFzaEZ1bmN0aW9uO3hwAAAABj+EeuFHrhR7AA6gKXNyADlpby5naXRodWIubWljaGFlbG1pb3IuYmxvb21maWx0ZXIuY29yZS5Sb2FyaW5nQml0U2V0QXJyYXli1WkanEeL8gIAAkkABHNpemVMAAZiaXRtYXB0ACFMb3JnL3JvYXJpbmdiaXRtYXAvUm9hcmluZ0JpdG1hcDt4cAAOoClzcgAfb3JnLnJvYXJpbmdiaXRtYXAuUm9hcmluZ0JpdG1hcAAAAAAAAAAGDAAAeHB3PDowAAAFAAAAAAAAAAQAAAAGAAEACwAAAA0AAAAwAAAAMgAAADQAAAA4AAAAOgAAAIb2Esp6c2B7VPhh5Xhwc3IAOmlvLmdpdGh1Yi5taWNoYWVsbWlvci5ibG9vbWZpbHRlci5oYXNoLk11cm11cjNIYXNoRnVuY3Rpb25KZz7AMRtAHwIAAHhw")
    )
    val bloomProp = intSchema.properties.get[IntBloomFilterProperty]
    bloomProp.bloomFilter.contains(BigInt(2).toByteArray) shouldBe true
  }
}

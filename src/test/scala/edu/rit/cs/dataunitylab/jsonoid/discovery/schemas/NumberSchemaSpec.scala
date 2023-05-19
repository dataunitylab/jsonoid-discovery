package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._
import org.scalactic.TolerantNumerics

import UnitSpec._

class NumberSchemaSpec extends UnitSpec {
  implicit val formats: Formats = DefaultFormats

  private val numberSchema =
    NumberSchema(3.14).merge(NumberSchema(4.28)).asInstanceOf[NumberSchema]
  private val numberSchemaIntersect =
    NumberSchema(3.5)
      .merge(NumberSchema(7.0), Intersect)
      .asInstanceOf[NumberSchema]
  private val integerSchema = IntegerSchema(5)
  private val mixedSchema =
    numberSchema.merge(integerSchema).asInstanceOf[NumberSchema]

  behavior of "MaxNumValueProperty"

  it should "track the maximum value" in {
    numberSchema.properties should contain(MaxNumValueProperty(Some(4.28)))
  }

  it should "track the maximum value on intersection" in {
    numberSchemaIntersect.properties should contain(
      MaxNumValueProperty(Some(3.5))
    )
  }

  it should "expand by incrementing values" in {
    MaxNumValueProperty(Some(11))
      .expandTo(Some(MaxNumValueProperty((Some(12)))))
      .maxNumValue shouldBe Some(12)
  }

  behavior of "MinNumValueProperty"

  it should "track the minimum value" in {
    numberSchema.properties should contain(MinNumValueProperty(Some(3.14)))
  }

  it should "track the minimum value on intersection" in {
    numberSchemaIntersect.properties should contain(
      MinNumValueProperty(Some(7.0))
    )
  }

  it should "expand by decrementing values" in {
    MinNumValueProperty(Some(11))
      .expandTo(Some(MinNumValueProperty((Some(10)))))
      .minNumValue shouldBe Some(10)
  }

  behavior of "NumHyperLogLogProperty"

  it should "track the distinct elements" in {
    val hyperLogLogProp = numberSchema.properties.get[NumHyperLogLogProperty]
    hyperLogLogProp.hll.count() should be(2)
  }

  behavior of "NumBloomFilterProperty"

  it should "keep a Bloom filter of observed elements" in {
    val bloomFilterProp = numberSchema.properties.get[NumBloomFilterProperty]
    bloomFilterProp.bloomFilter.contains(
      BigDecimal(3.14).toString.getBytes
    ) shouldBe true
  }

  behavior of "NumStatsProperty"

  it should "keep statistics" in {
    val statsProp = numberSchema.properties.get[NumStatsProperty]
    statsProp.stats.mean shouldBe (BigDecimal(3.71))
  }

  behavior of "NumExamplesProperty"

  it should "keep examples" in {
    val examplesProp = numberSchema.properties.get[NumExamplesProperty]
    (examplesProp.toJson() \ "examples") shouldEqual JArray(
      List(BigDecimal(3.14), BigDecimal(4.28))
    )
  }

  behavior of "NumMultipleOfProperty"

  it should "track a common multiple" in {
    val multipleProp = numberSchema.properties.get[NumMultipleOfProperty]
    multipleProp.multiple.value shouldBe (0.02)
  }

  it should "track a common multiple on intersection" in {
    val multipleProp =
      numberSchemaIntersect.properties.get[NumMultipleOfProperty]
    multipleProp.multiple.value shouldBe (7.0)
  }

  it should "not track multiples of zero" in {
    val zeroNumSchema =
      NumberSchema(0).merge(NumberSchema(0)).asInstanceOf[NumberSchema]
    val multipleProp = zeroNumSchema.properties.get[NumMultipleOfProperty]
    multipleProp.toJson() shouldBe JObject()
  }

  it should "not track multiples of very small values" in {
    val zeroNumSchema =
      NumberSchema(1e-11).merge(NumberSchema(2e-11)).asInstanceOf[NumberSchema]
    val multipleProp = zeroNumSchema.properties.get[NumMultipleOfProperty]
    multipleProp.toJson() shouldBe JObject()
  }

  it should "not crash with numbers of very different scales" in {
    val p1 = NumMultipleOfProperty(Some(2.42e+244))
    val p2 = NumMultipleOfProperty(Some(44221.0))
    p1.unionMerge(p2).toJson() shouldBe JObject()
  }

  it should "be compatible with the same multiple" in {
    NumMultipleOfProperty(Some(BigDecimal(3.0))).isSubsetOf(
      NumMultipleOfProperty(Some(BigDecimal(3.0)))
    ) shouldBe true
  }

  it should "not be compatible with a larger multiple" in {
    NumMultipleOfProperty(Some(BigDecimal(3.0))).isSubsetOf(
      NumMultipleOfProperty(Some(BigDecimal(6.0)))
    ) shouldBe false
  }

  it should "be compatible with a smaller multiple" in {
    NumMultipleOfProperty(Some(BigDecimal(4.0))).isSubsetOf(
      NumMultipleOfProperty(Some(BigDecimal(2.0)))
    ) shouldBe true
  }

  it should "be compatible if no multiple" in {
    NumMultipleOfProperty(Some(BigDecimal(2.0))).isSubsetOf(
      NumMultipleOfProperty(None)
    ) shouldBe true
  }

  it should "not be compatible with a zero multiple" in {
    NumMultipleOfProperty(Some(14.0)).isSubsetOf(
      NumMultipleOfProperty(Some(0.0))
    ) shouldBe false
  }

  it should "be compatible with multiples of different scales" in {
    val numSchema1 = NumberSchema(2.0)
    val numSchema2 = NumberSchema(BigDecimal("888888.8888888889"))
    val mergedSchema = numSchema1.merge(numSchema2).asInstanceOf[NumberSchema]
    numSchema1.isSubsetOf(mergedSchema) shouldBe true
    numSchema2.isSubsetOf(mergedSchema) shouldBe true
  }

  it should "expand by division by 2s" in {
    NumMultipleOfProperty(Some(4.0))
      .expandTo(Some(NumMultipleOfProperty((Some(2.0)))))
      .multiple shouldBe Some(2.0)
  }

  it should "stop expanding after MaxExpandRounds" in {
    NumMultipleOfProperty(Some(1048576.0))
      .expandTo(Some(NumMultipleOfProperty((Some(2.0)))))
      .multiple shouldBe None
  }

  it should "not expand if already covered" in {
    NumMultipleOfProperty(Some(3.5))
      .expandTo(Some(NumMultipleOfProperty((Some(7.0)))))
      .multiple shouldBe Some(3.5)
  }

  behavior of "NumHistogramProperty"

  it should "keep a running histogram" in {
    implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(0.02)

    val histProp = numberSchema.properties.get[NumHistogramProperty]
    val histJson = histProp.toJson() \ "histogram"
    val bins = (histJson \ "bins").extract[List[List[Double]]]
    bins(0)(0) should ===(3.14)
    bins(0)(1) should ===(1.0)
    bins(1)(0) should ===(4.28)
    bins(1)(1) should ===(1.0)
    (histJson \ "hasExtremeValues").extract[Boolean] shouldBe false
  }

  behavior of "NumberSchema"

  it should "track the maximum value when merged with an integer schema" in {
    mixedSchema.properties should contain(MaxNumValueProperty(Some(5)))
  }

  it should "track the minimum value when merged with an integer schema" in {
    mixedSchema.properties should contain(MinNumValueProperty(Some(3.14)))
  }

  it should "track the distinct elements when merged with an integer schema" in {
    val hyperLogLogProp = mixedSchema.properties.get[NumHyperLogLogProperty]
    hyperLogLogProp.hll.count() should be(3)
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
    (examplesProp.toJson() \ "examples") shouldEqual JArray(
      List(BigDecimal(3.14), BigDecimal(4.28), BigDecimal(5))
    )
  }

  it should "keep a running histogram when merged with an integer schema" in {
    implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(0.02)

    val histProp = mixedSchema.properties.get[NumHistogramProperty]
    val histJson = histProp.toJson() \ "histogram"
    val bins = (histJson \ "bins").extract[List[List[Double]]]
    bins(0)(0) should ===(3.14)
    bins(0)(1) should ===(1.0)
    bins(1)(0) should ===(4.28)
    bins(1)(1) should ===(1.0)
    (histJson \ "hasExtremeValues").extract[Boolean] shouldBe false
  }

  it should "have no properties in the minimal property set" in {
    NumberSchema(0.0)(
      JsonoidParams().withPropertySet(PropertySets.MinProperties)
    ).properties shouldBe empty
  }

  it should "show integers as a valid type" in {
    val int: JValue = JInt(3)
    numberSchema.isValidType(int) shouldBe (true)
  }

  it should "show numbers as a valid type" in {
    val dbl: JValue = JDouble(3.4)
    numberSchema.isValidType(dbl) shouldBe (true)
  }

  it should "show decimals a valid type" in {
    val dec: JValue = JDecimal(3.4)
    numberSchema.isValidType(dec) shouldBe (true)
  }

  it should "show strings as an invalid type" in {
    numberSchema.isValidType(JString("foo")).shouldBe(false)
  }

  it should "not detect anomalies when a double is in range" in {
    numberSchema.collectAnomalies(JDouble(3.14)) shouldBe empty
  }

  it should "not detect anomalies when a decimal is in range" in {
    numberSchema.collectAnomalies(JDecimal(3.14)) shouldBe empty
  }

  it should "not detect anomalies when an integer is in range" in {
    mixedSchema.collectAnomalies(JInt(5)) shouldBe empty
  }

  it should "not detect anomalies for non-numerical values" in {
    mixedSchema.properties.flatMap(
      _.collectAnomalies(JString("foo"))
    ) shouldBe empty
  }

  it should "detect anomalies when a value is too small" in {
    numberSchema.properties
      .get[MinNumValueProperty]
      .collectAnomalies(JInt(3)) shouldBe Seq(
      Anomaly("$", "value is below minimum", AnomalyLevel.Warning)
    )
  }

  it should "detect anomalies when a value is too large" in {
    numberSchema.properties
      .get[MaxNumValueProperty]
      .collectAnomalies(JInt(50)) shouldBe Seq(
      Anomaly("$", "value is above maximum", AnomalyLevel.Warning)
    )
  }

  it should "detect anomalies when a value has not been observed" in {
    numberSchema.properties
      .get[NumBloomFilterProperty]
      .collectAnomalies(JInt(4)) shouldBe Seq(
      Anomaly("$", "value not found in Bloom filter", AnomalyLevel.Info)
    )
  }

  it should "detect anomalies when a value outside of the histogram range" in {
    numberSchema.properties
      .get[NumHistogramProperty]
      .collectAnomalies(JInt(30)) shouldBe Seq(
      Anomaly("$", "value outside histogram bounds", AnomalyLevel.Info)
    )
  }

  it should "be compatible with a similar integer schema" in {
    NumberSchema(1.0).isSubsetOf(IntegerSchema(1)) shouldBe true
  }

  it should "be compatible with a matching schema" in {
    NumberSchema(1.0).isSubsetOf(NumberSchema(1.0)) shouldBe true
  }

  it should "expand to be compatible with a similar schema" in {
    val schema = NumberSchema(2)
    schema.isSubsetOf(NumberSchema(1).expandTo(Some(schema))) shouldBe true
  }
}

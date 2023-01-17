package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._
import org.scalactic.TolerantNumerics

import PropertySets._
import UnitSpec._

class NumberSchemaSpec extends UnitSpec {
  behavior of "NumberSchema"

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

  it should "track the maximum value" in {
    numberSchema.properties should contain(MaxNumValueProperty(Some(4.28)))
  }

  it should "track the maximum value on intersection" in {
    numberSchemaIntersect.properties should contain(
      MaxNumValueProperty(Some(3.5))
    )
  }

  it should "track the minimum value" in {
    numberSchema.properties should contain(MinNumValueProperty(Some(3.14)))
  }

  it should "track the minimum value on intersection" in {
    numberSchemaIntersect.properties should contain(
      MinNumValueProperty(Some(7.0))
    )
  }

  it should "track the distinct elements" in {
    val hyperLogLogProp = numberSchema.properties.get[NumHyperLogLogProperty]
    hyperLogLogProp.hll.count() should be(2)
  }

  it should "keep a Bloom filter of observed elements" in {
    val bloomFilterProp = numberSchema.properties.get[NumBloomFilterProperty]
    bloomFilterProp.bloomFilter.contains(
      BigDecimal(3.14).toString.getBytes
    ) shouldBe true
  }

  it should "keep statistics" in {
    val statsProp = numberSchema.properties.get[NumStatsProperty]
    statsProp.stats.mean shouldBe (BigDecimal(3.71))
  }

  it should "keep examples" in {
    val examplesProp = numberSchema.properties.get[NumExamplesProperty]
    (examplesProp.toJson \ "examples") shouldEqual JArray(
      List(BigDecimal(3.14), BigDecimal(4.28))
    )
  }

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
    multipleProp.toJson shouldBe JObject()
  }

  it should "keep a running histogram" in {
    implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(0.02)

    val histProp = numberSchema.properties.get[NumHistogramProperty]
    val bins = (histProp.toJson \ "histogram").extract[List[List[Double]]]
    bins(0)(0) should ===(3.14)
    bins(0)(1) should ===(1.0)
    bins(1)(0) should ===(4.28)
    bins(1)(1) should ===(1.0)
  }

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
    (examplesProp.toJson \ "examples") shouldEqual JArray(
      List(BigDecimal(3.14), BigDecimal(4.28), BigDecimal(5))
    )
  }

  it should "keep a running histogram when merged with an integer schema" in {
    implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(0.02)

    val histProp = mixedSchema.properties.get[NumHistogramProperty]
    val bins = (histProp.toJson \ "histogram").extract[List[List[Double]]]
    bins(0)(0) should ===(3.14)
    bins(0)(1) should ===(1.0)
    bins(1)(0) should ===(4.28)
    bins(1)(1) should ===(1.0)
  }

  it should "have no properties in the minimal property set" in {
    NumberSchema(0.0)(
      PropertySets.MinProperties,
      JsonoidParams.defaultJsonoidParams
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
      Anomaly("$", "value is below minimum", Warning)
    )
  }

  it should "detect anomalies when a value is too large" in {
    numberSchema.properties
      .get[MaxNumValueProperty]
      .collectAnomalies(JInt(50)) shouldBe Seq(
      Anomaly("$", "value is above maximum", Warning)
    )
  }

  it should "detect anomalies when a value has not been observed" in {
    numberSchema.properties
      .get[NumBloomFilterProperty]
      .collectAnomalies(JInt(4)) shouldBe Seq(
      Anomaly("$", "value not found in Bloom filter", Info)
    )
  }

  it should "detect anomalies when a value outside of the histogram range" in {
    numberSchema.properties
      .get[NumHistogramProperty]
      .collectAnomalies(JInt(30)) shouldBe Seq(
      Anomaly("$", "value outside histogram bounds", Warning)
    )
  }
}

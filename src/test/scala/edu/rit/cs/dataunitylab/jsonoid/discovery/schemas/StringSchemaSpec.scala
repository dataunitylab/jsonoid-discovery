package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

import org.json4s.{DefaultFormats, Formats}
import org.json4s._

import UnitSpec._

class StringSchemaSpec extends UnitSpec {
  implicit val formats: Formats = DefaultFormats

  private val stringSchema = StringSchema(
    StringSchema("foor").properties.mergeValue("foobar")
  )

  behavior of "MaxLengthProperty"

  it should "track the maximum length" in {
    stringSchema.properties should contain(MaxLengthProperty(Some(6)))
  }

  behavior of "MinLengthProperty"

  it should "track the minimum length" in {
    stringSchema.properties should contain(MinLengthProperty(Some(4)))
  }

  behavior of "StringHyperLogLogProperty"

  it should "track the distinct elements" in {
    val hyperLogLogProp = stringSchema.properties.get[StringHyperLogLogProperty]
    hyperLogLogProp.hll.count() should be(2)
  }

  behavior of "StringBloomFilterProperty"

  it should "keep a Bloom filter of observed elements" in {
    val bloomFilterProp = stringSchema.properties.get[StringBloomFilterProperty]
    bloomFilterProp.bloomFilter.contains("foor") shouldBe true
  }

  behavior of "StringExamplesProperty"

  it should "keep examples" in {
    val examplesProp = stringSchema.properties.get[StringExamplesProperty]
    (examplesProp.toJson() \ "examples")
      .extract[List[String]] should contain theSameElementsAs List(
      "foor",
      "foobar"
    )
  }

  behavior of "FormatProperty"

  def schemaWithFormat(value: String, format: String): Unit = {
    it should s"detect the ${format} format" in {
      val p = JsonoidParams().withExtendedFormats(true)
      var props = StringSchema(value)(p).properties
      for (_ <- 1 to 10) { props = props.mergeValue(value)(p) }
      props = props.mergeValue("foofoofoofoofoofoo")
      val formatProp = props.get[FormatProperty]

      // With a reasonable threshold, detect the format
      (formatProp.toJson()(
        JsonoidParams().withFormatThreshold(0.9f)
      ) \ "format").extract[String] shouldBe format

      // With default threshold, no format
      (formatProp.toJson() \ "format") shouldBe JNothing
    }
  }

  schemaWithFormat("127.0.0.1", "ipv4")
  schemaWithFormat("::1", "ipv6")
  schemaWithFormat("foo@bar.com", "email")
  schemaWithFormat("http://example.com", "uri")
  schemaWithFormat("2008-11-13", "date")
  schemaWithFormat("2018-11-13T20:20:39+00:00", "date-time")
  schemaWithFormat("20:20:39+00:00", "time")
  schemaWithFormat("01020304-0506-0708-090a-0b0c0d0e0f10", "uuid")
  schemaWithFormat("1991ASSL..171...89H", "bibcode")
  schemaWithFormat("1-56619-909-3", "isbn")
  schemaWithFormat("10.1093/ajae/aaq063", "doi")
  schemaWithFormat("8FWC2345+G6", "plus-code")
  schemaWithFormat("geo:40.714623,-74.006605,1.1", "geo-uri")

  it should "not detect a format if most values match no format" in {
    var props = StringSchema().properties
    for (_ <- 1 to 10) { props = props.mergeValue("::1") }
    for (_ <- 1 to 500) { props = props.mergeValue("foo") }
    val formatProp = props.get[FormatProperty]
    (formatProp.toJson() \ "format") shouldBe JNothing
  }

  it should "not assign a format to normal text" in {
    val formatProp = stringSchema.properties.get[FormatProperty]
    (formatProp.toJson() \ "format") shouldBe JNothing
  }

  it should "not expand for compatible formats" in {
    FormatProperty(Map("url" -> 100))
      .expandTo(Some(FormatProperty(Map("url" -> 10))))
      .maxFormat() shouldBe Some("url")
  }

  it should "expand to remove formats if needed" in {
    FormatProperty(Map("url" -> 100))
      .expandTo(Some(FormatProperty(Map("email" -> 10))))
      .maxFormat() shouldBe None
  }

  behavior of "StaticPatternProperty"

  it should "should not allow merging regex properties" in {
    val regexProp = StaticPatternProperty("foo.*".r)
    an[UnsupportedOperationException] should be thrownBy
      regexProp.unionMerge(regexProp)
  }

  it should "should not consider matching values anomalous" in {
    val regexProp = StaticPatternProperty("foo.*".r)
    regexProp.isAnomalous(JString("barfoobaz")) shouldBe false
  }

  it should "should find an anomaly for a non-matching string" in {
    val regexProp = StaticPatternProperty("foo.*".r)
    regexProp.collectAnomalies(JString("bar")) shouldBe List(
      Anomaly(
        "$",
        "value does not match the required regex",
        AnomalyLevel.Fatal
      )
    )
  }

  behavior of "PatternProperty"

  it should "should find common prefixes" in {
    var prefixSchema = StringSchema("foobar").properties
    for (_ <- 1 to 10) { prefixSchema = prefixSchema.mergeValue("foobaz") }
    val prefixProp = prefixSchema.get[PatternProperty]
    (prefixProp.toJson() \ "pattern").extract[String] shouldBe "^fooba"
  }

  it should "should find common prefixes with a newline" in {
    var prefixSchema = StringSchema("foo\r\nbar").properties
    for (_ <- 1 to 10) { prefixSchema = prefixSchema.mergeValue("foo\r\nbaz") }
    val prefixProp = prefixSchema.get[PatternProperty]
    (prefixProp.toJson() \ "pattern").extract[String] shouldBe "^foo\r\nba"
  }

  it should "should not generate a pattern with no prefixes" in {
    var randomSchema = StringSchema().properties
    for (c <- 'a' to 'z') { randomSchema = randomSchema.mergeValue(c.toString) }
    val prefixProp = randomSchema.get[PatternProperty]
    (prefixProp.toJson() \ "pattern").extractOpt[String] shouldBe None
  }

  it should "should find prefixes and suffixes together" in {
    var prefixSchema = StringSchema("barfoo").properties
    for (_ <- 1 to 10) { prefixSchema = prefixSchema.mergeValue("bazfoo") }
    val prefixProp = prefixSchema.get[PatternProperty]
    (prefixProp.toJson() \ "pattern").extract[String] shouldBe "^ba.*foo$"
  }

  it should "expand to remove patterns if needed" in {
    val newPattern =
      PatternProperty(Some("foo"), Some("bar"), 100, Some(20)).expandTo(
        Some(PatternProperty())
      )
    newPattern.prefix shouldBe None
    newPattern.suffix shouldBe None
  }

  it should "not expand to remove patterns if not needed" in {
    val newPattern =
      PatternProperty(Some("fo"), Some("ar")).expandTo(
        Some(PatternProperty(Some("foo"), Some("bar")))
      )
    newPattern.prefix shouldBe Some("fo")
    newPattern.suffix shouldBe Some("ar")
  }

  it should "not report anomalies with valid strings" in {
    val pattern = PatternProperty(Some("fo"), Some("ar"))
    pattern.collectAnomalies(JString("foobar")) shouldBe empty
  }

  it should "detect anomalies when a string does not have the required prefix" in {
    val pattern =
      PatternProperty(Some("foo"), None, PatternProperty.MinExamples, Some(10))
    pattern.collectAnomalies(JString("quux")) shouldBe List(
      Anomaly(
        "$",
        "value does not have the required prefix",
        AnomalyLevel.Fatal
      )
    )
  }

  it should "detect anomalies when a string does not have the required suffix" in {
    val pattern =
      PatternProperty(None, Some("foo"), PatternProperty.MinExamples, Some(10))
    pattern.collectAnomalies(JString("quux")) shouldBe List(
      Anomaly(
        "$",
        "value does not have the required suffix",
        AnomalyLevel.Fatal
      )
    )
  }

  it should "report an anomaly for strings without a matching prefix" in {
    val pattern =
      PatternProperty(Some("foo"), None, PatternProperty.MinExamples, Some(10))
    pattern
      .collectAnomalies(JString("bar")) should contain theSameElementsAs (List(
      Anomaly(
        "$",
        "value does not have the required prefix",
        AnomalyLevel.Fatal
      )
    ))
  }

  it should "report an anomaly for strings without a matching suffix" in {
    val pattern =
      PatternProperty(None, Some("bar"), PatternProperty.MinExamples, Some(10))
    pattern
      .collectAnomalies(JString("foo")) should contain theSameElementsAs (List(
      Anomaly(
        "$",
        "value does not have the required suffix",
        AnomalyLevel.Fatal
      )
    ))
  }

  it should "report anomalies with Warning level for numeric strings" in {
    var pattern = PatternProperty(
      Some("12"),
      Some("34"),
      PatternProperty.MinExamples,
      Some(10)
    )
    pattern
      .collectAnomalies(JString("0"))
      .map(_.anomalyLevel)
      .max shouldBe AnomalyLevel.Warning
  }

  behavior of "StringLengthHistogramProperty"

  it should "keep a running histogram of lengths" in {
    val histProp = stringSchema.properties.get[StringLengthHistogramProperty]
    val bins =
      (histProp.toJson() \ "lengthHistogram").extract[List[List[Double]]]
    bins(0)(0) should equal(4.0 +- 0.1)
    bins(0)(1) should ===(1.0)
    bins(1)(0) should ===(6.0 +- 0.1)
    bins(1)(1) should ===(1.0)
  }

  behavior of "StringNumericProperty"

  it should "produce numeric schemas for valid strings" in {
    val numProp = StringNumericProperty().mergeValue("3.2")
    numProp.numericSchema shouldNot be(empty)
  }

  it should "not produce numeric schemas when an invalid value is observed" in {
    val numProp = StringNumericProperty().mergeValue("3.2").mergeValue("foo")
    numProp.numericSchema shouldBe empty
  }

  it should "show non-numeric strings as anomalous" in {
    val numProp = StringNumericProperty().mergeValue("3.2")
    numProp.isAnomalous(JString("foo")) shouldBe true
  }

  it should "show not show numeric strings as anomalous" in {
    val numProp = StringNumericProperty().mergeValue("3.2")
    numProp.collectAnomalies(JString("3.2")) shouldBe empty
  }

  behavior of "StringSchema"

  it should "have no properties in the minimal property set" in {
    StringSchema("foo")(
      JsonoidParams().withPropertySet(PropertySets.MinProperties)
    ).properties shouldBe empty
  }

  it should "show strings as a valid type" in {
    val str: JValue = JString("foo")
    stringSchema.isValidType(str).shouldBe(true)
  }

  it should "show numbers as an invalid type" in {
    stringSchema.isValidType(JDouble(3.4)) shouldBe (false)
  }

  it should "not detect anomalies for a valid string" in {
    stringSchema.collectAnomalies(JString("foor")) shouldBe empty
  }

  it should "not detect anomalies for a non-string value" in {
    stringSchema.properties.flatMap(_.collectAnomalies(JInt(3))) shouldBe empty
  }

  it should "detect anomalies when a string is too short" in {
    stringSchema.properties
      .get[MinLengthProperty]
      .collectAnomalies(JString("a")) should contain(
      Anomaly("$", "string shorter than minimum length", AnomalyLevel.Warning)
    )
  }

  it should "detect anomalies when a string is too long" in {
    stringSchema.properties
      .get[MaxLengthProperty]
      .collectAnomalies(JString("foobarbaz")) should contain(
      Anomaly("$", "string longer than maximum length", AnomalyLevel.Warning)
    )
  }

  it should "not detect anomalies for an observed value" in {
    stringSchema.properties
      .get[StringBloomFilterProperty]
      .collectAnomalies(JString("foor")) shouldBe empty
  }

  it should "detect anomalies when a value has not been observed" in {
    stringSchema.properties
      .get[StringBloomFilterProperty]
      .collectAnomalies(JString("quux")) should contain(
      Anomaly("$", "value not found in Bloom filter", AnomalyLevel.Info)
    )
  }

  it should "detect anomalies when a string length outside of the histogram range" in {
    stringSchema.properties
      .get[StringLengthHistogramProperty]
      .collectAnomalies(JString("foobarbazquux")) should contain(
      Anomaly(
        "$",
        "string length outside histogram range",
        AnomalyLevel.Info
      )
    )
  }

  it should "detect anomalies when a string does not match a regex" in {
    val anomalies = StaticPatternProperty("foo.*".r)
      .collectAnomalies(JString("bar"))
    anomalies shouldBe List(
      Anomaly(
        "$",
        "value does not match the required regex",
        AnomalyLevel.Fatal
      )
    )
  }

  it should "be compatible with a matching schema" in {
    StringSchema("foo").isSubsetOf(StringSchema("foo")) shouldBe true
  }

  it should "expand to be compatible with a similar schema" in {
    val schema = StringSchema("a")
    schema.isSubsetOf(StringSchema("foo").expandTo(Some(schema))) shouldBe true
  }

  it should "convert numeric string schemas to numbers" in {
    val propSet = PropertySets.MinProperties
    propSet.stringProperties.add(StringNumericProperty())

    (StringSchema("3.2")(
      JsonoidParams().withPropertySet(propSet)
    ).toJson() \ "type").extract[String] shouldBe "number"
  }
}

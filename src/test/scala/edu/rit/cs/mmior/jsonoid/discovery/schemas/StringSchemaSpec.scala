package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.{DefaultFormats, Formats}
import org.json4s._

import PropertySets._
import UnitSpec._

class StringSchemaSpec extends UnitSpec {
  behavior of "StringSchema"

  implicit val formats: Formats = DefaultFormats

  private val stringSchema = StringSchema(StringSchema("foor").properties.mergeValue("foobar"))

  it should "track the maximum length" in {
    stringSchema.properties should contain (MaxLengthProperty(Some(6)))
  }

  it should "track the minimum length" in {
    stringSchema.properties should contain (MinLengthProperty(Some(4)))
  }

  it should "track the distinct elements" in {
    val hyperLogLogProp = stringSchema.properties.get[StringHyperLogLogProperty]
    hyperLogLogProp.hll.count() should be (2)
  }

  it should "keep a Bloom filter of observed elements" in {
    val bloomFilterProp = stringSchema.properties.get[StringBloomFilterProperty]
    bloomFilterProp.bloomFilter.contains("foor") shouldBe true
  }

  it should "keep examples" in {
    val examplesProp = stringSchema.properties.get[StringExamplesProperty]
    (examplesProp.toJson \ "examples").extract[List[String]] should contain theSameElementsAs List("foor", "foobar")
  }

  it should "detect the IPv4 format" in {
    val ipSchema = StringSchema("127.0.0.1")
    val formatProp = ipSchema.properties.get[FormatProperty]
    (formatProp.toJson \ "format").extract[String] shouldBe "ipv4"
  }

  it should "detect the IPv6 format" in {
    val ipSchema = StringSchema("::1")
    val formatProp = ipSchema.properties.get[FormatProperty]
    (formatProp.toJson \ "format").extract[String] shouldBe "ipv6"
  }

  it should "detect the email format" in {
    val ipSchema = StringSchema("foo@bar.com")
    val formatProp = ipSchema.properties.get[FormatProperty]
    (formatProp.toJson \ "format").extract[String] shouldBe "email"
  }

  it should "detect the uri format" in {
    val ipSchema = StringSchema("http://example.com")
    val formatProp = ipSchema.properties.get[FormatProperty]
    (formatProp.toJson \ "format").extract[String] shouldBe "uri"
  }

  it should "detect the date format" in {
    val dateSchema = StringSchema("2008-11-13")
    val formatProp = dateSchema.properties.get[FormatProperty]
    (formatProp.toJson \ "format").extract[String] shouldBe "date"
  }

  it should "detect the date-time format" in {
    val dateTimeSchema = StringSchema("2018-11-13T20:20:39+00:00")
    val formatProp = dateTimeSchema.properties.get[FormatProperty]
    (formatProp.toJson \ "format").extract[String] shouldBe "date-time"
  }

  it should "detect the time format" in {
    val timeSchema = StringSchema("20:20:39+00:00")
    val formatProp = timeSchema.properties.get[FormatProperty]
    (formatProp.toJson \ "format").extract[String] shouldBe "time"
  }

  it should "detect the uuid format" in {
    val timeSchema = StringSchema("01020304-0506-0708-090a-0b0c0d0e0f10")
    val formatProp = timeSchema.properties.get[FormatProperty]
    (formatProp.toJson \ "format").extract[String] shouldBe "uuid"
  }

  it should "not assign a format to normal text" in {
    val formatProp = stringSchema.properties.get[FormatProperty]
    (formatProp.toJson \ "format") shouldBe JNothing
  }

  it should "should find common prefixes" in {
    @SuppressWarnings(Array("org.wartremover.warts.Var"))
    var prefixSchema = StringSchema("foobar").properties
    for (_ <- 1 to 10) { prefixSchema = prefixSchema.mergeValue("foobaz") }
    val prefixProp = prefixSchema.get[PatternProperty]
    (prefixProp.toJson \ "pattern").extract[String] shouldBe "^fooba"
  }

  it should "should not generate a pattern with no prefixes" in {
    @SuppressWarnings(Array("org.wartremover.warts.Var"))
    var randomSchema = StringSchema().properties
    for (c <- 'a' to 'z') { randomSchema = randomSchema.mergeValue(c.toString) }
    val prefixProp = randomSchema.get[PatternProperty]
    (prefixProp.toJson \ "pattern").extractOpt[String] shouldBe None
  }

  it should "should find prefixes and suffixes together" in {
    @SuppressWarnings(Array("org.wartremover.warts.Var"))
    var prefixSchema = StringSchema("barfoo").properties
    for (_ <- 1 to 10) { prefixSchema = prefixSchema.mergeValue("bazfoo") }
    val prefixProp = prefixSchema.get[PatternProperty]
    (prefixProp.toJson \ "pattern").extract[String] shouldBe "^ba.*foo$"
  }

  it should "have no properties in the minimal property set" in {
    StringSchema("foo")(PropertySets.MinProperties).properties shouldBe empty
  }

  it should "keep a running histogram of lengths" in {
    val histProp = stringSchema.properties.get[StringLengthHistogramProperty]
    histProp.histogram.bins shouldBe List((4, 1), (6, 1))
  }

  it should "show strings as a valid type" in {
    stringSchema.isValidType(JString("foo")).shouldBe (true)
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
    stringSchema.properties.get[MinLengthProperty].collectAnomalies(JString("a")) should contain (Anomaly("$", "string shorter than minimum length", Warning))
  }

  it should "detect anomalies when a string is too long" in {
    stringSchema.properties.get[MaxLengthProperty].collectAnomalies(JString("foobarbaz")) should contain (Anomaly("$", "string longer than maximum length", Warning))
  }

  it should "not detect anomalies for an observed value" in {
    stringSchema.properties.get[StringBloomFilterProperty].collectAnomalies(JString("foor")) shouldBe empty
  }

  it should "detect anomalies when a value has not been observed" in {
    stringSchema.properties.get[StringBloomFilterProperty].collectAnomalies(JString("quux")) should contain (Anomaly("$", "value not found in Bloom filter", Info))
  }

  it should "detect anomalies when a string length outside of the histogram range" in {
    stringSchema.properties.get[StringLengthHistogramProperty].collectAnomalies(JString("foobarbazquux")) should contain (Anomaly("$", "string length outside histogram range", Warning))
  }

  it should "detect anomalies when a string does not have the required prefix" in {
    val anomalies = stringSchema.properties.get[PatternProperty].collectAnomalies(JString("quuxr"))
    anomalies shouldBe List(Anomaly("$", "value does not have the required prefix", Fatal))
  }

  it should "detect anomalies when a string does not have the required suffix" in {
    val anomalies = stringSchema.properties.get[PatternProperty].collectAnomalies(JString("foo"))
    anomalies shouldBe List(Anomaly("$", "value does not have the required suffix", Fatal))
  }
}

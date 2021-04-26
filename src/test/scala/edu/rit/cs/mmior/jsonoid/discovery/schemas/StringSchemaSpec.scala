package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._

import UnitSpec._

class StringSchemaSpec extends UnitSpec {
  behavior of "StringSchema"

  private val stringSchema = StringSchema("foo").properties.mergeValue("foobar")

  it should "track the maximum length" in {
    stringSchema should contain (MaxLengthProperty(Some(6)))
  }

  it should "track the minimum length" in {
    stringSchema should contain (MinLengthProperty(Some(3)))
  }

  it should "track the distinct elements" in {
    val hyperLogLogProp = stringSchema.get[StringHyperLogLogProperty]
    hyperLogLogProp.hll.count() should be (2)
  }

  it should "keep a Bloom filter of observed elements" in {
    val bloomFilterProp = stringSchema.get[StringBloomFilterProperty]
    bloomFilterProp.bloomFilter.contains("foo") shouldBe true
  }

  it should "keep examples" in {
    val examplesProp = stringSchema.get[StringExamplesProperty]
    (examplesProp.toJson \ "examples") shouldEqual JArray(List("foo", "foobar"))
  }

  it should "detect the IPv4 format" in {
    val ipSchema = StringSchema("127.0.0.1")
    val formatProp = ipSchema.properties.get[FormatProperty]
    (formatProp.toJson \ "format") shouldBe JString("ipv4")
  }

  it should "detect the IPv6 format" in {
    val ipSchema = StringSchema("::1")
    val formatProp = ipSchema.properties.get[FormatProperty]
    (formatProp.toJson \ "format") shouldBe JString("ipv6")
  }

  it should "detect the email format" in {
    val ipSchema = StringSchema("foo@bar.com")
    val formatProp = ipSchema.properties.get[FormatProperty]
    (formatProp.toJson \ "format") shouldBe JString("email")
  }

  it should "detect the uri format" in {
    val ipSchema = StringSchema("http://example.com")
    val formatProp = ipSchema.properties.get[FormatProperty]
    (formatProp.toJson \ "format") shouldBe JString("uri")
  }

  it should "detect the date format" in {
    val dateSchema = StringSchema("2008-11-13")
    val formatProp = dateSchema.properties.get[FormatProperty]
    (formatProp.toJson \ "format") shouldBe JString("date")
  }

  it should "detect the date-time format" in {
    val dateTimeSchema = StringSchema("2018-11-13T20:20:39+00:00")
    val formatProp = dateTimeSchema.properties.get[FormatProperty]
    (formatProp.toJson \ "format") shouldBe JString("date-time")
  }

  it should "detect the time format" in {
    val timeSchema = StringSchema("20:20:39+00:00")
    val formatProp = timeSchema.properties.get[FormatProperty]
    (formatProp.toJson \ "format") shouldBe JString("time")
  }

  it should "not assign a format to normal text" in {
    val formatProp = stringSchema.get[FormatProperty]
    (formatProp.toJson \ "format") shouldBe JNothing
  }
}

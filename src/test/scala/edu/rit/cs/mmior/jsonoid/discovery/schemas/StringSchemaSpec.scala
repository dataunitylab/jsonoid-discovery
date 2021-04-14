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
}

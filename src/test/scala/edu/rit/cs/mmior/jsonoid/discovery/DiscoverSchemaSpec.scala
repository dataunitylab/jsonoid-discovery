package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s._


class DiscoverSchemaSpec extends UnitSpec {
  behavior of "DiscoverSchema"

  it should "produce an array schema" in {
    DiscoverSchema.discoverFromValue(JArray(List(JBool(true)))) shouldBe a [ArraySchema]
  }

  it should "produce an array schema from a set" in {
    DiscoverSchema.discoverFromValue(JSet(Set(JBool(true)))) shouldBe a [ArraySchema]
  }

  it should "produce a boolean schema" in {
    DiscoverSchema.discoverFromValue(JBool(true)) shouldBe a [BooleanSchema]
  }

  it should "produce a number schema" in {
    DiscoverSchema.discoverFromValue(JDecimal(1.0)) shouldBe a [NumberSchema]
  }

  it should "produce an integer schema" in {
    DiscoverSchema.discoverFromValue(JInt(1)) shouldBe a [IntegerSchema]
  }

  it should "produce a null schema" in {
    DiscoverSchema.discoverFromValue(JNull) shouldBe a [NullSchema]
  }

  it should "produce an object schema" in {
    DiscoverSchema.discoverFromValue(JObject(List(("foo", JBool(true))))) shouldBe a [ObjectSchema]
  }

  it should "produce a string schema" in {
    DiscoverSchema.discoverFromValue(JString("foo")) shouldBe a [StringSchema]
  }
}

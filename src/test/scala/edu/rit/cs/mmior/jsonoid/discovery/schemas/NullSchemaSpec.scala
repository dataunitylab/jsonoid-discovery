package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s._

import PropertySets._

class NullSchemaSpec extends UnitSpec {
  behavior of "NullSchema"

  val nullSchema: NullSchema = NullSchema()

  it should "show nulls as a valid type" in {
    nullSchema.isValidType(JNull) shouldBe (true)
  }

  it should "show non-nulls as an invalid type" in {
    nullSchema.isValidType(JString("foo")) shouldBe (false)
  }

  it should "not be compatible with other schemas" in {
    nullSchema.isCompatibleWith(StringSchema("foo")) shouldBe false
  }
}

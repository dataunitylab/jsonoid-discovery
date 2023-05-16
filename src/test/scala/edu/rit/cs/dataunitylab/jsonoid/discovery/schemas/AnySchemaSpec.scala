package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

import org.json4s._

class AnySchemaSpec extends UnitSpec {
  behavior of "AnySchema"

  private val anySchema = AnySchema()

  it should "have type 'any'" in {
    anySchema.schemaType shouldBe "any"
  }

  it should "produce a schema accepting anything" in {
    anySchema.toJson() shouldBe JObject()
  }

  it should "should all types as valid" in {
    anySchema.isValidType(JArray(List())) shouldBe (true)
    anySchema.isValidType(JBool(true)) shouldBe (true)
    anySchema.isValidType(JDecimal(2.5)) shouldBe (true)
    anySchema.isValidType(JDouble(2.5)) shouldBe (true)
    anySchema.isValidType(JInt(3)) shouldBe (true)
    anySchema.isValidType(JLong(3)) shouldBe (true)
    anySchema.isValidType(JNothing) shouldBe (true)
    anySchema.isValidType(JNull) shouldBe (true)
    anySchema.isValidType(JObject()) shouldBe (true)
    anySchema.isValidType(JString("foo")) shouldBe (true)
  }

  it should "be compatible with other schemas" in {
    StringSchema("foo").isSubsetOf(anySchema) shouldBe true
  }
}

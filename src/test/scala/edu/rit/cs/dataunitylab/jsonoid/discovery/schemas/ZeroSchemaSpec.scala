package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

import org.json4s.JsonDSL._
import org.json4s._

class ZeroSchemaSpec extends UnitSpec {
  behavior of "ZeroSchema"

  private val zeroSchema = ZeroSchema()

  it should "have type 'zero'" in {
    zeroSchema.schemaType shouldBe "zero"
  }

  it should "produce an empty schema" in {
    val emptySchema: JObject = ("not" -> Nil)
    zeroSchema.toJson() shouldBe emptySchema
  }

  it should "have no valid types" in {
    zeroSchema.validTypes shouldBe empty
  }

  behavior of "expandTo"

  it should "not expand when given another ZeroSchema" in {
    zeroSchema.expandTo(Some(zeroSchema)) shouldBe zeroSchema
  }

  it should "expand to AnySchema when given None" in {
    zeroSchema.expandTo(None) shouldBe AnySchema()
  }

  it should "expand a default instance of any other schema" in {
    zeroSchema.expandTo(Some(IntegerSchema())) shouldBe an[IntegerSchema]
  }
}

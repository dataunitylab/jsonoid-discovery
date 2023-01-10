package edu.rit.cs.mmior.jsonoid.discovery
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
    zeroSchema.toJson shouldBe emptySchema
  }

  it should "have no valid types" in {
    zeroSchema.validTypes shouldBe empty
  }
}

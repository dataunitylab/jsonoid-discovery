package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s._

class BooleanSchemaSpec extends UnitSpec {
  behavior of "BooleanSchema"

  val booleanSchema: BooleanSchema = BooleanSchema()

  it should "show booleans as a valid type" in {
    booleanSchema.isValidType(JBool(true)) shouldBe (true)
  }

  it should "show non-booleans as an invalid type" in {
    booleanSchema.isValidType(JString("foo")) shouldBe (false)
  }

  it should "detect no anomalies with a non-boolean type" in {
    booleanSchema.properties.flatMap(_.collectAnomalies(JDouble(3.4))) shouldBe empty
  }
}

package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s._

class EnumSchemaSpec extends UnitSpec {
  behavior of "EnumSchema"

  val values: Set[JValue] = Set(JString("foo"), JString("bar"))
  val enumSchema: EnumSchema = EnumSchema(values)

  it should "not detect anamolies for values in the given set" in {
    enumSchema.collectAnomalies(JString("foo")) shouldBe empty
  }

  it should "detect anamolies for values not in the given set" in {
    enumSchema.collectAnomalies(JString("quux")) shouldBe List(Anomaly("$", "enum value not found", Fatal))
  }
}

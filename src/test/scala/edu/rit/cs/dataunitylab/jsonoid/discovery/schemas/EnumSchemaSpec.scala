package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

import org.json4s._

class EnumSchemaSpec extends UnitSpec {
  behavior of "EnumSchema"

  val values: Set[JValue] = Set(JString("foo"), JString("bar"))
  val enumSchema: EnumSchema = EnumSchema(values)

  val smallValues: Set[JValue] = Set(JString("foo"))
  val smallEnumSchema: EnumSchema = EnumSchema(smallValues)

  val bigValues: Set[JValue] =
    Set(JString("foo"), JString("bar"), JString("baz"))
  val bigEnumSchema: EnumSchema = EnumSchema(bigValues)

  it should "not detect anamolies for values in the given set" in {
    enumSchema.collectAnomalies(JString("foo")) shouldBe empty
  }

  it should "detect anamolies for values not in the given set" in {
    enumSchema.collectAnomalies(JString("quux")) shouldBe List(
      Anomaly("$", "enum value not found", AnomalyLevel.Fatal)
    )
  }

  it should "be compatible with a smaller enum" in {
    enumSchema.isCompatibleWith(smallEnumSchema) shouldBe true
  }

  it should "not be compatible with a larger enum" in {
    enumSchema.isCompatibleWith(bigEnumSchema) shouldBe false
  }
}

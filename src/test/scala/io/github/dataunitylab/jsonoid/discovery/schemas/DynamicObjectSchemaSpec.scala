package io.github.dataunitylab.jsonoid.discovery
package schemas

import org.json4s._

import UnitSpec.pointerFromString

class DynamicObjectSchemaSpec extends UnitSpec {
  private val objectSchema = DynamicObjectSchema(BooleanSchema())

  behavior of "DynamicObjectSchema"

  it should "be able to find subschemas by pointer" in {
    objectSchema.findByPointer("/foo").value shouldBe BooleanSchema()
  }

  it should "not detect anomalies for valid values" in {
    objectSchema
      .isAnomalous(JObject(List(("foo", JBool(true)))))
      .shouldBe(false)
  }

  it should "detect anomalies for invalid values" in {
    objectSchema
      .isAnomalous(JObject(List(("foo", JString("bar")))))
      .shouldBe(true)
  }

  it should "not detect anomalies for non-object values" in {
    objectSchema.properties.flatMap(
      _.collectAnomalies(JString("foo"))
    ) shouldBe empty
  }
}

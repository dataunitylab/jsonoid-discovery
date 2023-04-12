package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

import org.json4s._

import UnitSpec._

class BooleanSchemaSpec extends UnitSpec {
  implicit val formats: Formats = DefaultFormats

  behavior of "BooleanSchema"

  private val booleanSchema = BooleanSchema()

  it should "have type 'boolean'" in {
    booleanSchema.schemaType shouldBe "boolean"
  }

  it should "have only boolean as a valid type" in {
    booleanSchema.validTypes shouldBe Set(classOf[JBool])
  }

  behavior of "BooleanPercentProperty"

  it should "track the percentage of true values" in {
    val pctProp = BooleanPercentProperty().mergeValue(true).mergeValue(false)
    (pctProp.toJson() \ "pctTrue").extract[Double] shouldBe 0.5
  }
}

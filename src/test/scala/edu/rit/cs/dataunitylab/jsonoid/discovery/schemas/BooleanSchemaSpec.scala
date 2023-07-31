package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

import org.json4s._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import UnitSpec._

class BooleanSchemaSpec extends UnitSpec with ScalaCheckPropertyChecks {
  implicit val formats: Formats = DefaultFormats

  behavior of "BooleanSchema"

  private val booleanSchema = BooleanSchema()

  it should "be a subset of itself" in {
    forAll(SchemaGen.genBoolSchema) { schema =>
      schema.isSubsetOf(schema).shouldBe(true)
    }
  }

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

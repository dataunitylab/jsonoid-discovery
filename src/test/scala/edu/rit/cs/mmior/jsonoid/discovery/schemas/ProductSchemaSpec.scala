package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.{DefaultFormats, Formats}
import org.json4s._

import UnitSpec._

class ProductSchemaSpec extends UnitSpec {
  behavior of "ProductSchema"

  implicit val formats: Formats = DefaultFormats

  private val schema1 = BooleanSchema()
  private val schema2 = IntegerSchema()
  private val schema3 = StringSchema()
  private val productSchema1 = ProductSchema(schema1).merge(schema2)
  private val productSchema2 = ProductSchema(schema3)

  it should "track required properties" in {
    productSchema1.properties should contain (ProductSchemaTypesProperty(Map(schema1.getClass
      -> schema1, schema2.getClass -> schema2)))
  }

  it should "merge all types in multiple ProductSchemas" in {
    ProductSchema(schema3).merge(productSchema1).properties should contain (ProductSchemaTypesProperty(Map(
      schema1.getClass -> schema1, schema2.getClass -> schema2, schema3.getClass -> schema3)))
  }

  it should "convert to JSON using anyOf" in {
    val anyTypes = (productSchema1.toJson \ "anyOf").extract[JArray]
    ((anyTypes \ "type")(0).extract[String], (anyTypes \ "type")(1).extract[String]) shouldBe ("boolean", "integer")
  }
}

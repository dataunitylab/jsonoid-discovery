package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.{DefaultFormats, Formats}
import org.json4s._

class ProductSchemaSpec extends UnitSpec {
  behavior of "ProductSchema"

  implicit val formats: Formats = DefaultFormats
  implicit val er: EquivalenceRelation = EquivalenceRelations.KindEquivalenceRelation
  implicit val propSet: PropertySet = PropertySets.MinProperties

  private val schema1 = BooleanSchema()
  private val schema2 = IntegerSchema()
  private val schema3 = StringSchema()
  private val productSchema1 = ProductSchema(schema1).merge(schema2).asInstanceOf[ProductSchema]
  private val productSchema2 = ProductSchema(schema3)

  it should "track required properties" in {
    val types = productSchema1.properties.get[ProductSchemaTypesProperty].schemaTypes
    types should contain theSameElementsAs List(schema1, schema2)
  }

  it should "merge all types in multiple ProductSchemas" in {
    val types = ProductSchema(schema3).merge(productSchema1).asInstanceOf[ProductSchema].properties.get[ProductSchemaTypesProperty].schemaTypes
    types should contain theSameElementsAs List(schema1, schema2, schema3)
  }

  it should "convert to JSON using oneOf" in {
    val anyTypes = (productSchema1.toJson \ "oneOf").extract[JArray]
    ((anyTypes \ "type")(0).extract[String], (anyTypes \ "type")(1).extract[String]) shouldBe ("boolean", "integer")
  }

  it should "allow replacement of a schema with a reference" in {
    val productSchema = ProductSchema(ObjectSchema(Map("foo" -> BooleanSchema()))).merge(BooleanSchema()).replaceWithReference("/0/foo", "foo")
    (((productSchema.toJson \ "oneOf")(0)) \ "properties" \ "foo").extract[Map[String, String]] shouldEqual Map("$ref" -> "foo")
  }
}

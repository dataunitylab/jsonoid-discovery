package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.{DefaultFormats, Formats}
import org.json4s._

class ProductSchemaSpec extends UnitSpec {
  behavior of "ProductSchema"

  implicit val formats: Formats = DefaultFormats
  implicit val er: EquivalenceRelation =
    EquivalenceRelations.KindEquivalenceRelation
  implicit val propSet: PropertySet = PropertySets.MinProperties

  private val schema1 = BooleanSchema()
  private val schema2 = IntegerSchema(0)
  private val schema3 = StringSchema()
  private val productSchema1 =
    ProductSchema(schema1).merge(schema2).asInstanceOf[ProductSchema]
  private val productSchema2 = ProductSchema(schema3)

  private val allProps = SchemaProperties
    .empty[JsonSchema[_]]
    .replaceProperty(
      ProductSchemaTypesProperty(List(StringSchema()), List(1), true)
    )
  private val allProductSchema = ProductSchema(allProps)

  it should "track required properties" in {
    val typesProp = productSchema1
      .merge(schema2)
      .asInstanceOf[ProductSchema]
      .properties
      .get[ProductSchemaTypesProperty]
    val types = typesProp.schemaTypes.zip(typesProp.schemaCounts)
    types should contain theSameElementsAs List((schema1, 1), (schema2, 2))
  }

  it should "merge all types in multiple ProductSchemas" in {
    val typesProp = ProductSchema(schema3)
      .merge(productSchema1)
      .asInstanceOf[ProductSchema]
      .properties
      .get[ProductSchemaTypesProperty]
    val types = typesProp.schemaTypes.zip(typesProp.schemaCounts)
    types should contain theSameElementsAs List(
      (schema1, 1),
      (schema2, 1),
      (schema3, 1)
    )
  }

  it should "not merge all types in multiple ProductSchemas with all" in {
    val mergedSchemaProps =
      allProductSchema
        .merge(allProductSchema)
        .asInstanceOf[ProductSchema]
        .properties
        .get[ProductSchemaTypesProperty]

    mergedSchemaProps.schemaTypes should contain theSameElementsAs List(
      StringSchema(),
      StringSchema()
    )
    mergedSchemaProps.all shouldBe true
  }

  it should "not created nested ProductSchemas with mixed all values" in {
    val mergedSchemaProps =
      allProductSchema
        .merge(productSchema1)
        .asInstanceOf[ProductSchema]
        .properties
        .get[ProductSchemaTypesProperty]

    mergedSchemaProps.schemaTypes should contain theSameElementsAs List(
      StringSchema(),
      productSchema1
    )
    mergedSchemaProps.all shouldBe true
  }

  it should "convert to JSON using oneOf" in {
    val anyTypes = (productSchema1.toJson \ "oneOf").extract[JArray]
    (
      (anyTypes \ "type")(0).extract[String],
      (anyTypes \ "type")(1).extract[String]
    ) shouldBe ("boolean", "integer")
  }

  it should "allow replacement of a schema with a reference" in {
    val productSchema = ProductSchema(
      ObjectSchema(Map("foo" -> BooleanSchema()))
    ).merge(BooleanSchema()).replaceWithReference("/0/foo", "foo")
    (((productSchema.toJson \ "oneOf")(0)) \ "properties" \ "foo")
      .extract[Map[String, String]] shouldEqual Map("$ref" -> "foo")
  }

  it should "transform types" in {
    val transformedSchema = productSchema1.transformProperties { case _ =>
      NullSchema()
    }
    transformedSchema
      .asInstanceOf[ProductSchema]
      .properties
      .get[ProductSchemaTypesProperty]
      .schemaTypes
      .shouldEqual(List(NullSchema(), NullSchema()))
  }

  it should "detect anomalies of incorrect type" in {
    productSchema1.isAnomalous(JString("foo")).shouldBe(true)
  }
}

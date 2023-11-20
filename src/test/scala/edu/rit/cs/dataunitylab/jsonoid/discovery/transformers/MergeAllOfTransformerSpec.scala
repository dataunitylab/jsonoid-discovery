package io.github.dataunitylab.jsonoid.discovery
package transformers

import schemas._

import UnitSpec._

class MergeAllOfTransformerSpec extends UnitSpec {
  behavior of "MergeAllOfTransformer"

  implicit val er: EquivalenceRelation =
    EquivalenceRelations.KindEquivalenceRelation

  it should "intersect properties of objects" in {
    val schemaProps =
      SchemaProperties
        .empty[JsonSchema[_]]
        .replaceProperty(
          ProductSchemaTypesProperty(
            AnySchema(),
            List(
              ObjectSchema(Map("foo" -> BooleanSchema())),
              ObjectSchema(
                Map("foo" -> BooleanSchema(), "bar" -> BooleanSchema())
              )
            ),
            List(1, 1),
            AllOf
          )
        )
    val productSchema = ProductSchema(schemaProps)
    val transformedSchema = MergeAllOfTransformer.transformSchema(productSchema)
    transformedSchema shouldBe an[ObjectSchema]

    val objectTypes = transformedSchema
      .asInstanceOf[ObjectSchema]
      .properties
      .get[ObjectTypesProperty]
      .objectTypes
    objectTypes should have size 1
    objectTypes("foo") shouldBe BooleanSchema()
  }

  it should "combine properties of strings" in {
    val schemaProps =
      SchemaProperties
        .empty[JsonSchema[_]]
        .replaceProperty(
          ProductSchemaTypesProperty(
            AnySchema(),
            List(
              StringSchema("foobar"),
              StringSchema("baz")
            ),
            List(1, 1),
            AllOf
          )
        )
    val productSchema = ProductSchema(schemaProps)
    val transformedSchema = MergeAllOfTransformer.transformSchema(productSchema)

    transformedSchema shouldBe a[StringSchema]

    val stringProps = transformedSchema.asInstanceOf[StringSchema].properties

    stringProps should contain(MinLengthProperty(Some(6)))
    stringProps should contain(MaxLengthProperty(Some(3)))
  }
}

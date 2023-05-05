package edu.rit.cs.dataunitylab.jsonoid.discovery
package transformers

import schemas._

class DisjointObjectTransformerSpec extends UnitSpec {
  behavior of "DisjointObjectTransformer"

  implicit val propSet = PropertySets.AllProperties

  it should "replace disjoint objects with a product schema" in {
    val obj1 =
      ObjectSchema(Map("foo" -> StringSchema(), "bar" -> StringSchema()))
    val obj2 =
      ObjectSchema(Map("baz" -> StringSchema(), "quux" -> StringSchema()))

    val transformedSchema =
      DisjointObjectTransformer.transformSchema(obj1.merge(obj2))
    transformedSchema shouldBe a[ProductSchema]
    transformedSchema
      .asInstanceOf[ProductSchema]
      .properties
      .get[ProductSchemaTypesProperty]
      .schemaTypes
      .map(
        _.asInstanceOf[ObjectSchema].properties
          .get[ObjectTypesProperty]
          .objectTypes
          .keys
      ) should contain theSameElementsAs Seq(
      Set("foo", "bar"),
      Set("baz", "quux")
    )
  }
}

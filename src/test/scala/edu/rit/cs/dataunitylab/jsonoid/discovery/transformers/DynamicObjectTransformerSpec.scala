package edu.rit.cs.dataunitylab.jsonoid.discovery
package transformers

import schemas._

class DynamicObjectTransformerSpec extends UnitSpec {
  behavior of "DynamicObjectTransformer"

  it should "replace dynamic objects" in {
    var props = ObjectSchema().properties
    for (i <- 1 to 11) {
      props = props.mergeValue(Map("a" * i -> StringSchema()))
    }

    val transformedSchema =
      DynamicObjectTransformer.transformSchema(ObjectSchema(props))
    transformedSchema shouldBe a[DynamicObjectSchema]
    transformedSchema
      .asInstanceOf[DynamicObjectSchema]
      .properties
      .get[DynamicObjectTypeProperty]
      .valueType shouldBe a[StringSchema]
  }

  it should "not replace objects with static keys" in {
    var props = ObjectSchema().properties
    for (_ <- 1 to 11) {
      props = props.mergeValue(Map("a" -> StringSchema()))
    }

    val transformedSchema =
      DynamicObjectTransformer.transformSchema(ObjectSchema(props))
    transformedSchema shouldBe ObjectSchema(props)
  }

  it should "not replace objects with different types" in {
    var props = ObjectSchema().properties
    for (i <- 1 to 11) {
      if (i % 2 === 0) {
        props = props.mergeValue(Map("a" * i -> StringSchema()))
      } else {
        props = props.mergeValue(Map("a" * i -> BooleanSchema()))
      }
    }

    val transformedSchema =
      DynamicObjectTransformer.transformSchema(ObjectSchema(props))
    transformedSchema shouldBe ObjectSchema(props)
  }
}

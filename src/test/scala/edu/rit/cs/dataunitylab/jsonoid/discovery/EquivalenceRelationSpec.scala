package edu.rit.cs.dataunitylab.jsonoid.discovery

import schemas._

class EquivalenceRelationSpec extends UnitSpec {

  implicit val propSet: PropertySet = PropertySets.MinProperties

  val objSchema1: ObjectSchema = ObjectSchema(Map("foo" -> BooleanSchema()))
  val objSchema2: ObjectSchema = ObjectSchema(Map("bar" -> BooleanSchema()))
  val objSchema3: ObjectSchema = ObjectSchema(
    Map("foo" -> BooleanSchema(), "bar" -> BooleanSchema())
  )
  val objSchema4: ObjectSchema = ObjectSchema(
    Map("foo" -> IntegerSchema(), "bar" -> BooleanSchema())
  )

  behavior of "KindEquivalenceRelation"

  it should "keep schemas separate when merging by label" in {
    val merged = objSchema1.merge(objSchema2)(
      JsonoidParams().withER(
        EquivalenceRelations.LabelEquivalenceRelation
      )
    )
    merged shouldBe a[ProductSchema]
  }

  it should "merge object schemas by kind" in {
    val merged = objSchema1
      .merge(objSchema2)(
        JsonoidParams()
          .withER(EquivalenceRelations.KindEquivalenceRelation)
      )
      .asInstanceOf[ObjectSchema]
    merged.properties.get[ObjectTypesProperty].objectTypes shouldBe Map(
      "foo" -> BooleanSchema(),
      "bar" -> BooleanSchema()
    )
  }

  behavior of "LabelEquivalenceRelation"

  it should "create ProductSchemas when merging non-objects" in {
    val merged = objSchema1
      .merge(IntegerSchema(0))(
        JsonoidParams().withER(
          EquivalenceRelations.LabelEquivalenceRelation
        )
      )
    merged shouldBe a[ProductSchema]
  }

  behavior of "NonEquivalenceRelation"

  it should "not merge when using non-equivalence" in {
    val merged =
      objSchema1.merge(objSchema1)(
        JsonoidParams().withER(
          EquivalenceRelations.NonEquivalenceRelation
        )
      )
    merged shouldBe a[ProductSchema]
  }

  behavior of "IntersectingLabelEquivalenceRelation"

  it should "separate schemas which have no common labels" in {
    val merged = objSchema1.merge(objSchema2)(
      JsonoidParams().withER(
        EquivalenceRelations.IntersectingLabelEquivalenceRelation
      )
    )
    merged shouldBe a[ProductSchema]
  }

  it should "keep schemas together which have common labels" in {
    val merged = objSchema1
      .merge(objSchema3)(
        JsonoidParams()
          .withER(EquivalenceRelations.IntersectingLabelEquivalenceRelation)
      )
      .asInstanceOf[ObjectSchema]
    merged.properties.get[ObjectTypesProperty].objectTypes shouldBe Map(
      "foo" -> BooleanSchema(),
      "bar" -> BooleanSchema()
    )
  }

  behavior of "TypeMatchEquivalenceRelation"

  it should "combine schemas with the same type" in {
    val merged = objSchema1
      .merge(objSchema3)(
        JsonoidParams()
          .withER(EquivalenceRelations.TypeMatchEquivalenceRelation)
      )
      .asInstanceOf[ObjectSchema]
    merged.properties.get[ObjectTypesProperty].objectTypes shouldBe Map(
      "foo" -> BooleanSchema(),
      "bar" -> BooleanSchema()
    )
  }

  it should "separate schemas which different types" in {
    val merged = objSchema1.merge(objSchema4)(
      JsonoidParams().withER(
        EquivalenceRelations.TypeMatchEquivalenceRelation
      )
    )
    merged shouldBe a[ProductSchema]
  }
}

package edu.rit.cs.mmior.jsonoid.discovery

import schemas._

class EquivalenceRelationSpec extends UnitSpec {
  behavior of "EquivalenceRelation"

  implicit val propSet: PropertySet = PropertySets.MinProperties

  val objSchema1: ObjectSchema = ObjectSchema(Map("foo" -> BooleanSchema()))
  val objSchema2: ObjectSchema = ObjectSchema(Map("bar" -> BooleanSchema()))
  val objSchema3: ObjectSchema = ObjectSchema(
    Map("foo" -> BooleanSchema(), "bar" -> BooleanSchema())
  )

  it should "keep schemas separate when merging by label" in {
    val merged = objSchema1.merge(objSchema2)(
      EquivalenceRelations.LabelEquivalenceRelation
    )
    merged shouldBe a[ProductSchema]
  }

  it should "merge object schemas by kind" in {
    val merged = objSchema1
      .merge(objSchema2)(EquivalenceRelations.KindEquivalenceRelation)
      .asInstanceOf[ObjectSchema]
    merged.properties.get[ObjectTypesProperty].objectTypes shouldBe Map(
      "foo" -> BooleanSchema(),
      "bar" -> BooleanSchema()
    )
  }

  it should "create ProductSchemas when merging non-objects" in {
    val merged = objSchema1
      .merge(IntegerSchema(0))(EquivalenceRelations.LabelEquivalenceRelation)
    merged shouldBe a[ProductSchema]
  }

  it should "not merge when using non-equivalence" in {
    val merged =
      objSchema1.merge(objSchema1)(EquivalenceRelations.NonEquivalenceRelation)
    merged shouldBe a[ProductSchema]
  }

  it should "separate schemas which have no common labels" in {
    val merged = objSchema1.merge(objSchema2)(
      EquivalenceRelations.IntersectingLabelEquivalenceRelation
    )
    merged shouldBe a[ProductSchema]
  }

  it should "keep schemas together which have common labels" in {
    val merged = objSchema1
      .merge(objSchema3)(
        EquivalenceRelations.IntersectingLabelEquivalenceRelation
      )
      .asInstanceOf[ObjectSchema]
    merged.properties.get[ObjectTypesProperty].objectTypes shouldBe Map(
      "foo" -> BooleanSchema(),
      "bar" -> BooleanSchema()
    )
  }
}

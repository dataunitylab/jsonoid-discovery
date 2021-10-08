package edu.rit.cs.mmior.jsonoid.discovery

import schemas._

class EquivalenceRelationSpec extends UnitSpec {
  behavior of "EquivalenceRelation"

  implicit val propSet: PropertySet = PropertySets.MinProperties

  val objSchema1: ObjectSchema = ObjectSchema(Map("foo" -> BooleanSchema()))
  val objSchema2: ObjectSchema = ObjectSchema(Map("bar" -> BooleanSchema()))

  it should "keep schemas separate when merging by label" in {
    val merged = objSchema1.merge(objSchema2)(EquivalenceRelations.LabelEquivalenceRelation)
    merged shouldBe a[ProductSchema]
  }

  it should "merge object schemas by kind" in {
    val merged = objSchema1.merge(objSchema2)(EquivalenceRelations.KindEquivalenceRelation)
    merged shouldEqual (ObjectSchema(Map("foo" -> BooleanSchema(), "bar" -> BooleanSchema())))
  }
}

package io.github.dataunitylab.jsonoid.discovery

import schemas._

class EquivalenceRelationSpec extends UnitSpec {
  val objSchema1: ObjectSchema = ObjectSchema(Map("foo" -> BooleanSchema()))
  val objSchema2: ObjectSchema = ObjectSchema(Map("bar" -> BooleanSchema()))
  val objSchema3: ObjectSchema = ObjectSchema(
    Map("foo" -> BooleanSchema(), "bar" -> BooleanSchema())
  )
  val objSchema4: ObjectSchema = ObjectSchema(
    Map("foo" -> IntegerSchema(), "bar" -> BooleanSchema())
  )

  behavior of "KindEquivalenceRelation"

  it should "keep schemas separate when merging by label" in withParams(er =
    EquivalenceRelations.LabelEquivalenceRelation
  ) { implicit params =>
    objSchema1.merge(objSchema2) shouldBe a[ProductSchema]
  }

  it should "merge object schemas by kind" in withParams(er =
    EquivalenceRelations.KindEquivalenceRelation
  ) { implicit params =>
    val merged = objSchema1
      .merge(objSchema2)
      .asInstanceOf[ObjectSchema]
    merged.properties.get[ObjectTypesProperty].objectTypes shouldBe Map(
      "foo" -> BooleanSchema(),
      "bar" -> BooleanSchema()
    )
  }

  behavior of "LabelEquivalenceRelation"

  it should "create ProductSchemas when merging non-objects" in withParams(er =
    EquivalenceRelations.LabelEquivalenceRelation
  ) { implicit params =>
    objSchema1.merge(IntegerSchema(0)) shouldBe a[ProductSchema]
  }

  behavior of "NonEquivalenceRelation"

  it should "not merge when using non-equivalence" in withParams(er =
    EquivalenceRelations.NonEquivalenceRelation
  ) { implicit params =>
    objSchema1.merge(objSchema1) shouldBe a[ProductSchema]
  }

  behavior of "IntersectingLabelEquivalenceRelation"

  it should "separate schemas which have no common labels" in withParams(er =
    EquivalenceRelations.IntersectingLabelEquivalenceRelation
  ) { implicit params =>
    objSchema1.merge(objSchema2) shouldBe a[ProductSchema]
  }

  it should "keep schemas together which have common labels" in withParams(er =
    EquivalenceRelations.IntersectingLabelEquivalenceRelation
  ) { implicit params =>
    val merged = objSchema1
      .merge(objSchema3)
      .asInstanceOf[ObjectSchema]
    merged.properties.get[ObjectTypesProperty].objectTypes shouldBe Map(
      "foo" -> BooleanSchema(),
      "bar" -> BooleanSchema()
    )
  }

  behavior of "TypeMatchEquivalenceRelation"

  it should "combine schemas with the same type" in withParams(er =
    EquivalenceRelations.TypeMatchEquivalenceRelation
  ) { implicit params =>
    val merged = objSchema1
      .merge(objSchema3)
      .asInstanceOf[ObjectSchema]
    merged.properties.get[ObjectTypesProperty].objectTypes shouldBe Map(
      "foo" -> BooleanSchema(),
      "bar" -> BooleanSchema()
    )
  }

  it should "separate schemas which different types" in withParams(er =
    EquivalenceRelations.TypeMatchEquivalenceRelation
  ) { implicit params =>
    objSchema1.merge(objSchema4) shouldBe a[ProductSchema]
  }
}

package edu.rit.cs.mmior.jsonoid.discovery

import scala.reflect.ClassTag

import schemas._

import PropertySets._

class IncompatibilityCollectorSpec extends UnitSpec {
  behavior of "IncompatibilityCollector"

  val schema1 = IntegerSchema(3)
  val schema2 = IntegerSchema(0)

  it should "find no incompaitiblities between a schema and itself" in {
    IncompatibilityCollector.findIncompatibilities(
      schema1,
      schema1
    ) shouldBe empty
  }

  it should "should find incompatibilities in different integer schemas" in {
    IncompatibilityCollector.findIncompatibilities(
      schema1,
      schema2
    ) should contain theSameElementsAs List(
      Incompatibility("$", ClassTag(classOf[MinIntValueProperty]))
    )
  }

  it should "should find incompatibilities with nested paths" in {
    IncompatibilityCollector.findIncompatibilities(
      ObjectSchema(Map("foo" -> schema1)),
      ObjectSchema(Map("foo" -> StringSchema("foo")))
    ) should contain theSameElementsAs List(
      Incompatibility("$.foo", ClassTag(classOf[ObjectTypesProperty]))
    )
  }

  it should "find incompatibilities in differing object schemas" in {
    val objectSchema1 = ObjectSchema(Map("foo" -> BooleanSchema()))
    val objectSchema2 = ObjectSchema(Map("bar" -> BooleanSchema()))

    IncompatibilityCollector.findIncompatibilities(
      objectSchema1,
      objectSchema2
    ) should contain theSameElementsAs List(
      Incompatibility("$", ClassTag(classOf[ObjectTypesProperty])),
      Incompatibility("$", ClassTag(classOf[RequiredProperty]))
    )
  }

  it should "find incompatibilities in different length tuple schemas" in {
    val schemaList1 = List(IntegerSchema(0), BooleanSchema(true))
    val tupleSchema1 = ArraySchema.tuple(schemaList1)

    val schemaList2 =
      List(IntegerSchema(0), BooleanSchema(true), StringSchema("foo"))
    val tupleSchema2 = ArraySchema.tuple(schemaList2)

    IncompatibilityCollector.findIncompatibilities(
      tupleSchema1,
      tupleSchema2
    ) should contain theSameElementsAs List(
      Incompatibility("$", ClassTag(classOf[ItemTypeProperty])),
      Incompatibility("$", ClassTag(classOf[MaxItemsProperty]))
    )
  }

  it should "find incompatibilities in differing tuple schemas" in {
    val schemaList1 = List(IntegerSchema(0), BooleanSchema(true))
    val tupleSchema1 = ArraySchema.tuple(schemaList1)

    val schemaList2 = List(IntegerSchema(0), StringSchema("foo"))
    val tupleSchema2 = ArraySchema.tuple(schemaList2)

    IncompatibilityCollector.findIncompatibilities(
      tupleSchema1,
      tupleSchema2
    ) should contain theSameElementsAs List(
      Incompatibility("$", ClassTag(classOf[ItemTypeProperty]))
    )
  }

  it should "find incompatibilities in differing array schemas" in {
    val arraySchema1 = ArraySchema.array(BooleanSchema())
    val arraySchema2 = ArraySchema.array(IntegerSchema(0))

    IncompatibilityCollector.findIncompatibilities(
      arraySchema1,
      arraySchema2
    ) should contain theSameElementsAs List(
      Incompatibility("$", ClassTag(classOf[ItemTypeProperty]))
    )
  }

  it should "find no incompatibilities with appropriate array/tuple schemas" in {
    val arraySchema = ArraySchema.array(BooleanSchema())
    val tupleSchema = ArraySchema.tuple(List(BooleanSchema()))

    IncompatibilityCollector.findIncompatibilities(
      arraySchema,
      tupleSchema
    ) shouldBe empty
  }
}

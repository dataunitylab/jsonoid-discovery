package io.github.dataunitylab.jsonoid.discovery

import scala.reflect.ClassTag
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import schemas._

class IncompatibilityCollectorSpec
    extends UnitSpec
    with ScalaCheckPropertyChecks {
  behavior of "IncompatibilityCollector"

  val schema1 = IntegerSchema(3)
  val schema2 = IntegerSchema(0)

  it should "find incompatibilities iff a schema is not a subset" in {
    forAll(SchemaGen.genObjectSchema, SchemaGen.genObjectSchema) {
      case (schema1, schema2) =>
        val incompats = IncompatibilityCollector.findIncompatibilities(
          schema1,
          schema2,
          skipIfSubset = false
        )
        incompats.isEmpty shouldEqual (schema1.isSubsetOf(schema2))
    }
  }

  it should "find no incompatiblities between a schema and itself" in {
    IncompatibilityCollector.findIncompatibilities(
      schema1,
      schema1
    ) shouldBe empty
  }

  it should "should find incompatibilities in different integer schemas" in {
    IncompatibilityCollector.findIncompatibilities(
      schema2,
      schema1
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
      Incompatibility("$", ClassTag(classOf[MinItemsProperty]))
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

    tupleSchema.isSubsetOf(arraySchema) shouldBe true
    arraySchema.isSubsetOf(tupleSchema) shouldBe false
    IncompatibilityCollector.findIncompatibilities(
      tupleSchema,
      arraySchema
    ) shouldBe empty
  }
}

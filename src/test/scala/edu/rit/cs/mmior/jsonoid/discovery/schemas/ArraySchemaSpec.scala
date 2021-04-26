package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import UnitSpec._

class ArraySchemaSpec extends UnitSpec {
  behavior of "ArraySchema"

  private val itemType = BooleanSchema()
  private val arraySchema = ArraySchema(List(itemType)).properties.mergeValue(List(itemType, itemType))

  it should "track item schemas" in {
    arraySchema should contain (ItemTypeProperty(Left(itemType)))
  }

  it should "track tuple schemas" in {
    val tupleItemSchemas = List(NullSchema(), BooleanSchema(true))
    val tupleSchema = ArraySchema(tupleItemSchemas).properties.mergeValue(tupleItemSchemas)
    tupleSchema should contain (ItemTypeProperty(Right(tupleItemSchemas)))
  }

  it should "track minimum array length" in {
    arraySchema should contain (MinArrayLengthProperty(Some(1)))
  }

  it should "track maximum array length" in {
    arraySchema should contain (MaxArrayLengthProperty(Some(2)))
  }

  it should "track whether string elements are unique" in {
    val schemaList: List[JsonSchema[_]] = List(StringSchema("foo"), StringSchema("bar"))
    val uniqueArraySchema = ArraySchema(schemaList)
    uniqueArraySchema.properties should contain (UniqueProperty(true))
  }

  it should "track whether integer elements are unique" in {
    val schemaList: List[JsonSchema[_]] = List(IntegerSchema(0),
      IntegerSchema(1))
    val uniqueArraySchema = ArraySchema(schemaList)
    uniqueArraySchema.properties should contain (UniqueProperty(true))
  }

  it should "track whether numeric elements are unique" in {
    val schemaList: List[JsonSchema[_]] = List(NumberSchema(1.0),
      NumberSchema(2.0))
    val uniqueArraySchema = ArraySchema(schemaList)
    uniqueArraySchema.properties should contain (UniqueProperty(true))
  }
}

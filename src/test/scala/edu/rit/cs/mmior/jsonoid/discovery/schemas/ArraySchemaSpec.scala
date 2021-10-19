package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import PropertySets._
import UnitSpec._

import org.json4s.{DefaultFormats, Formats}

class ArraySchemaSpec extends UnitSpec {
  behavior of "ArraySchema"

  implicit val formats: Formats = DefaultFormats

  private val itemType = BooleanSchema()
  private val arraySchema = ArraySchema(List(itemType)).properties.mergeValue(List(itemType, itemType))
  private val schemaList = List(NullSchema(), BooleanSchema(true))
  private val tupleSchema = ArraySchema(ArraySchema(schemaList).properties.mergeValue(schemaList))

  it should "track item schemas" in {
    arraySchema should contain (ItemTypeProperty(Left(itemType)))
  }

  it should "track tuple schemas" in {
    val tupleItemSchemas = List(NullSchema(), BooleanSchema(true))
    val tupleSchema = ArraySchema(tupleItemSchemas).properties.mergeValue(tupleItemSchemas)
    tupleSchema should contain (ItemTypeProperty(Right(tupleItemSchemas)))
  }

  it should "track minimum array length" in {
    arraySchema should contain (MinItemsProperty(Some(1)))
  }

  it should "track maximum array length" in {
    arraySchema should contain (MaxItemsProperty(Some(2)))
  }

  it should "not consider single element lists unique" in {
    val schemaList: List[JsonSchema[_]] = List(StringSchema("foo"))
    val uniqueArraySchema = ArraySchema(schemaList)
    uniqueArraySchema.properties should contain (UniqueProperty(true, true))
  }

  it should "track whether string elements are unique" in {
    val schemaList: List[JsonSchema[_]] = List(StringSchema("foo"), StringSchema("bar"))
    val uniqueArraySchema = ArraySchema(schemaList)
    uniqueArraySchema.properties should contain (UniqueProperty(true, false))
  }

  it should "track whether integer elements are unique" in {
    val schemaList: List[JsonSchema[_]] = List(IntegerSchema(0),
      IntegerSchema(1))
    val uniqueArraySchema = ArraySchema(schemaList)
    uniqueArraySchema.properties should contain (UniqueProperty(true, false))
  }

  it should "track whether numeric elements are unique" in {
    val schemaList: List[JsonSchema[_]] = List(NumberSchema(1.0),
      NumberSchema(2.0))
    val uniqueArraySchema = ArraySchema(schemaList)
    uniqueArraySchema.properties should contain (UniqueProperty(true, false))
  }

  it should "be able to find subschemas by pointer" in {
    tupleSchema.findByPointer("/1") shouldBe Some(BooleanSchema())
  }

  it should "be able to find nested subschemas by pointer" in {
    val nestedList = List(tupleSchema, tupleSchema)
    val nestedSchema = ArraySchema(ArraySchema(nestedList).properties.mergeValue(nestedList))
    nestedSchema.findByPointer("/0/1") shouldBe Some(BooleanSchema())
  }

  it should "have no properties in the minimal property set" in {
    val cp = new Checkpoint()

    val arrayProperties = ArraySchema(List(BooleanSchema()))(PropertySets.MinProperties).properties

    cp { arrayProperties should have size 1 }
    cp { arrayProperties.get[ItemTypeProperty] }

    cp.reportAll()
  }

  it should "allow replacement of a schema with a reference in a tuple schema" in {
    val refSchema = tupleSchema.replaceWithReference("/0", "foo")
    (refSchema.toJson \ "items").extract[List[Map[String, String]]] shouldEqual List(Map("$ref" -> "foo"), Map("type" -> "boolean"))
  }

  it should "allow replacement of a schema with a reference in a array schema" in {
    val arraySchema = ArraySchema(List(NullSchema())).merge(ArraySchema(List(NullSchema(), NullSchema())))
    val refSchema = arraySchema.replaceWithReference("/*", "foo")
    (refSchema.toJson \ "items").extract[Map[String, String]] shouldEqual Map("$ref" -> "foo")
  }

  it should "allow replacement of a schema with a reference in a nested array schema" in {
    val arraySchema = ArraySchema(List(tupleSchema)).merge(ArraySchema(List(tupleSchema, tupleSchema)))
    val refSchema = arraySchema.replaceWithReference("/*/0", "foo")
    (refSchema.toJson \ "items" \ "items")(0).extract[Map[String, String]] shouldEqual Map("$ref" -> "foo")
  }

  it should "keep a running histogram of array lengths" in {
    val histProp = arraySchema.get[ArrayLengthHistogramProperty]
    histProp.histogram.bins shouldBe List((1, 1), (2, 1))
  }
}

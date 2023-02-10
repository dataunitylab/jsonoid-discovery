package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import PropertySets._
import UnitSpec._

import org.json4s._
import org.json4s.{DefaultFormats, Formats}
import org.scalactic.TolerantNumerics

class ArraySchemaSpec extends UnitSpec {
  implicit val formats: Formats = DefaultFormats

  private val itemType = BooleanSchema()
  private val arraySchema = ArraySchema(
    ArraySchema(List(itemType)).properties.mergeValue(List(itemType, itemType))
  )
  private val schemaList = List(IntegerSchema(0), BooleanSchema(true))
  private val tupleSchema = ArraySchema.tuple(schemaList)

  behavior of "MinItemsProperty"

  it should "track minimum array length" in {
    arraySchema.properties should contain(MinItemsProperty(Some(1)))
  }

  it should "expand by decrementing" in {
    MinItemsProperty(Some(3))
      .expandTo(
        MinItemsProperty(Some(2))
      )
      .minItems shouldBe Some(2)
  }

  behavior of "MaxItemsProperty"

  it should "track maximum array length" in {
    arraySchema.properties should contain(MaxItemsProperty(Some(2)))
  }

  it should "expand by incrementing" in {
    MaxItemsProperty(Some(2))
      .expandTo(
        MaxItemsProperty(Some(3))
      )
      .maxItems shouldBe Some(3)
  }

  behavior of "UniqueProperty"

  it should "not consider single element lists unique" in {
    val schemaList: List[JsonSchema[_]] = List(StringSchema("foo"))
    val uniqueArraySchema = ArraySchema(schemaList)
    uniqueArraySchema.properties should contain(UniqueProperty(true, true))
  }

  it should "track whether string elements are unique" in {
    val schemaList: List[JsonSchema[_]] =
      List(StringSchema("foo"), StringSchema("bar"))
    val uniqueArraySchema = ArraySchema(schemaList)
    uniqueArraySchema.properties should contain(UniqueProperty(true, false))
    (uniqueArraySchema.toJson \ "uniqueItems").extract[Boolean] shouldBe (true)
  }

  it should "track whether integer elements are unique" in {
    val schemaList: List[JsonSchema[_]] =
      List(IntegerSchema(0), IntegerSchema(1))
    val uniqueArraySchema = ArraySchema(schemaList)
    uniqueArraySchema.properties should contain(UniqueProperty(true, false))
  }

  it should "track whether numeric elements are unique" in {
    val schemaList: List[JsonSchema[_]] =
      List(NumberSchema(1.0), NumberSchema(2.0))
    val uniqueArraySchema = ArraySchema(schemaList)
    uniqueArraySchema.properties should contain(UniqueProperty(true, false))
  }

  it should "not expand if both are unique" in {
    UniqueProperty(true, false).expandTo(
      UniqueProperty(true, false)
    ) shouldBe UniqueProperty(true, false)
  }

  it should "expand to remove uniqueness" in {
    UniqueProperty(true, false).expandTo(
      UniqueProperty(false, false)
    ) shouldBe UniqueProperty(false, false)
  }

  behavior of "ArraySchema"

  it should "track item schemas" in {
    arraySchema.properties should contain(ItemTypeProperty(Left(itemType)))
  }

  it should "track tuple schemas" in {
    val tupleItemSchemas = List(NullSchema(), BooleanSchema(true))
    val tupleSchema =
      ArraySchema(tupleItemSchemas).properties.mergeValue(tupleItemSchemas)
    tupleSchema should contain(ItemTypeProperty(Right(tupleItemSchemas)))
  }

  it should "be able to find subschemas by pointer" in {
    tupleSchema.findByPointer("/1") shouldBe Some(BooleanSchema())
  }

  it should "be able to find nested subschemas by pointer" in {
    val nestedList = List(tupleSchema, tupleSchema)
    val nestedSchema =
      ArraySchema(ArraySchema(nestedList).properties.mergeValue(nestedList))
    nestedSchema.findByPointer("/0/1") shouldBe Some(BooleanSchema())
  }

  it should "transform array schemas" in {
    val transformedSchema = arraySchema.transformProperties { case _ =>
      NullSchema()
    }
    transformedSchema
      .asInstanceOf[ArraySchema]
      .properties
      .get[ItemTypeProperty]
      .itemType
      .shouldEqual(Left(NullSchema()))
  }

  it should "transform tuple schemas" in {
    val transformedSchema = tupleSchema.transformProperties { case _ =>
      NullSchema()
    }
    transformedSchema
      .asInstanceOf[ArraySchema]
      .properties
      .get[ItemTypeProperty]
      .itemType
      .shouldEqual(Right(List(NullSchema(), NullSchema())))
  }

  it should "have no properties in the minimal property set" in {
    val cp = new Checkpoint()

    val arrayProperties =
      ArraySchema(List(BooleanSchema()))(
        PropertySets.MinProperties,
        JsonoidParams()
      ).properties

    cp { arrayProperties should have size 1 }
    cp { arrayProperties.get[ItemTypeProperty] }

    cp.reportAll()
  }

  it should "allow replacement of a schema with a reference in a tuple schema" in {
    val refSchema = tupleSchema.replaceWithReference("/0", "foo")
    (refSchema.toJson \ "items")
      .extract[List[Map[String, String]]] shouldEqual List(
      Map("$ref" -> "foo"),
      Map("type" -> "boolean")
    )
  }

  it should "allow replacement of a schema with a reference in a array schema" in {
    val arraySchema = ArraySchema(List(NullSchema()))
      .merge(ArraySchema(List(NullSchema(), NullSchema())))
    val refSchema = arraySchema.replaceWithReference("/*", "foo")
    (refSchema.toJson \ "items").extract[Map[String, String]] shouldEqual Map(
      "$ref" -> "foo"
    )
  }

  it should "allow replacement of a schema with a reference in a nested array schema" in {
    val arraySchema = ArraySchema(List(tupleSchema)).merge(
      ArraySchema(List(tupleSchema, tupleSchema))
    )
    val refSchema = arraySchema.replaceWithReference("/*/0", "foo")
    (refSchema.toJson \ "items" \ "items")(0)
      .extract[Map[String, String]] shouldEqual Map("$ref" -> "foo")
  }

  it should "keep a running histogram of array lengths" in {
    implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(0.02)

    val histProp = arraySchema.properties.get[ArrayLengthHistogramProperty]
    val bins = (histProp.toJson \ "lengthHistogram").extract[List[List[Double]]]
    bins(0)(0) should ===(1.0)
    bins(0)(1) should ===(1.0)
    bins(1)(0) should ===(2.0)
    bins(1)(1) should ===(1.0)
  }

  it should "find nothing in an array schema with an empty pointer" in {
    arraySchema.findByPointer("").shouldBe(None)
  }

  it should "find the single type in an array schema" in {
    arraySchema.findByPointer("/*").shouldEqual(Some(BooleanSchema()))
  }

  it should "find the array schema itself" in {
    arraySchema.findByPointer("/").shouldEqual(Some(arraySchema))
  }

  it should "find nothing in a tuple schema with an empty pointer" in {
    tupleSchema.findByPointer("").shouldBe(None)
  }

  it should "find a type in a tuple schema" in {
    tupleSchema.findByPointer("/1").shouldEqual(Some(BooleanSchema()))
  }

  it should "find the tuple schema itself" in {
    tupleSchema.findByPointer("/").shouldEqual(Some(tupleSchema))
  }

  it should "not show anomalies in for non-array values" in {
    arraySchema.properties.flatMap(
      _.collectAnomalies(JString("foo"))
    ) shouldBe empty
  }

  it should "not show anomalies in array schemas with the correct type" in {
    arraySchema.properties
      .get[ItemTypeProperty]
      .isAnomalous(JArray(List(JBool(true))))
      .shouldBe(false)
  }

  it should "detect type anomalies in array schemas" in {
    arraySchema.properties
      .get[ItemTypeProperty]
      .isAnomalous(JArray(List(JInt(3))))
      .shouldBe(true)
  }

  it should "not show anomalies in tuple schemas with the correct type" in {
    tupleSchema.properties
      .get[ItemTypeProperty]
      .isAnomalous(JArray(List(JInt(0), JBool(true))))
      .shouldBe(false)
  }

  it should "detect type anomalies in tuple schemas" in {
    tupleSchema.properties
      .get[ItemTypeProperty]
      .isAnomalous(JArray(List(JNull, JInt(3))))
      .shouldBe(true)
  }

  it should "detect anomalies when tuple schemas are the wrong length" in {
    tupleSchema.properties
      .get[ItemTypeProperty]
      .collectAnomalies(JArray(List(JNull))) shouldBe Seq(
      Anomaly("$", "wrong length for tuple schema", Fatal)
    )
  }

  it should "detect no anomalies when arrays are within length bounds" in {
    arraySchema.collectAnomalies(JArray(List(JBool(true)))) shouldBe empty
  }

  it should "detect anomalies when the array is too small" in {
    arraySchema.properties
      .get[MinItemsProperty]
      .collectAnomalies(JArray(List())) shouldBe Seq(
      Anomaly("$", "array smaller than minimum length", Warning)
    )
  }

  it should "detect anomalies when the array is too large" in {
    arraySchema.properties
      .get[MaxItemsProperty]
      .collectAnomalies(
        JArray(List(JBool(true), JBool(false), JBool(true)))
      ) shouldBe Seq(Anomaly("$", "array larger than maximum length", Warning))
  }

  it should "detect anomalies when the array is too large via histogram" in {
    arraySchema.properties
      .get[ArrayLengthHistogramProperty]
      .collectAnomalies(
        JArray(List(JBool(true), JBool(false), JBool(true), JBool(false)))
      ) shouldBe Seq(
      Anomaly("$", "array length outside histogram bounds", Warning)
    )
  }

  it should "detect anomalies when array elements are not unique" in {
    val schemaList: List[JsonSchema[_]] =
      List(StringSchema("foo"), StringSchema("bar"))
    val uniqueArraySchema = ArraySchema(schemaList)
    uniqueArraySchema.properties
      .get[UniqueProperty]
      .collectAnomalies(
        JArray(List(JString("foo"), JString("foo")))
      ) shouldBe Seq(Anomaly("$", "array items are not unique", Fatal))
  }

  it should "be compatible with a matching schema" in {
    ArraySchema(List(BooleanSchema()))
      .isCompatibleWith(ArraySchema(List(BooleanSchema()))) shouldBe true
  }

  it should "show tuple schemas with matching types as compatible" in {
    tupleSchema.isCompatibleWith(tupleSchema) shouldBe true
  }

  it should "show tuple schemas with mismatched types as not compatible with array schemas" in {
    arraySchema.isCompatibleWith(tupleSchema) shouldBe false
  }

  it should "show tuple schemas with matching types as compatible with array schemas" in {
    val productSchema = ProductSchema(IntegerSchema(0))
      .merge(BooleanSchema())
      .asInstanceOf[ProductSchema]
    val arraySchema = ArraySchema(
      ArraySchema(List(productSchema)).properties
        .mergeValue(List(productSchema, productSchema))
    )
    arraySchema.isCompatibleWith(tupleSchema) shouldBe true
  }

  it should "expand to be compatible with a similar array schema" in {
    val schema = ArraySchema.array(IntegerSchema(0))
    ArraySchema
      .array(IntegerSchema(1))
      .expandTo(schema)
      .isCompatibleWith(schema) shouldBe true
  }

  it should "expand a tuple schema to be compatible with an array" in {
    val schema = ArraySchema.array(IntegerSchema(0))
    ArraySchema
      .tuple(List(IntegerSchema(0), IntegerSchema(1)))
      .expandTo(schema)
      .isCompatibleWith(schema) shouldBe true
  }

  it should "expand a tuple schema to be compatible with another tuple" in {
    val schema = ArraySchema.tuple(List(IntegerSchema(1), IntegerSchema(2)))
    ArraySchema
      .tuple(List(IntegerSchema(3), IntegerSchema(4)))
      .expandTo(schema)
      .isCompatibleWith(schema) shouldBe true
  }

  it should "expand tuple schemas of different sizes to an array" in {
    val schema = ArraySchema.tuple(
      List(IntegerSchema(0), IntegerSchema(1), IntegerSchema(2))
    )
    ArraySchema
      .tuple(List(IntegerSchema(3), IntegerSchema(4)))
      .expandTo(schema)
      .isCompatibleWith(schema) shouldBe true
  }
}

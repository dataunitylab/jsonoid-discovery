package io.github.dataunitylab.jsonoid.discovery
package schemas

import org.json4s.{DefaultFormats, Formats}
import org.json4s._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import UnitSpec._

class ProductSchemaSpec extends UnitSpec with ScalaCheckPropertyChecks {
  behavior of "ProductSchema"

  implicit val formats: Formats = DefaultFormats
  implicit val propSet: PropertySet = PropertySets.MinProperties

  private val schema1 = BooleanSchema()
  private val schema2 = IntegerSchema(0)
  private val schema3 = StringSchema()
  private val productSchema1 =
    ProductSchema(schema1).merge(schema2).asInstanceOf[ProductSchema]
  private val productSchema2 = ProductSchema(schema3)

  private val allProps = SchemaProperties
    .empty[JsonSchema[_]]
    .replaceProperty(
      ProductSchemaTypesProperty(
        AnySchema(),
        List(StringSchema()),
        List(1),
        AllOf
      )
    )
  private val allProductSchema = ProductSchema(allProps)

  it should "be a subset of itself" in {
    forAll(SchemaGen.genProductSchema) { schema =>
      schema.isSubsetOf(schema).shouldBe(true)
    }
  }

  it should "always create merged values which are subsets" in {
    forAll(SchemaGen.genProductSchema, SchemaGen.genProductSchema) {
      case (schema1, schema2) =>
        val mergedSchema = schema1.merge(schema2).asInstanceOf[ProductSchema]
        schema1.isSubsetOf(mergedSchema).shouldBe(true)
        schema2.isSubsetOf(mergedSchema).shouldBe(true)
    }
  }

  it should "be a subset iff there are incompatibilities" in {
    forAll(SchemaGen.genProductSchema, SchemaGen.genProductSchema) {
      case (schema1, schema2) =>
        val incompatibilities = schema1.findIncompatibilities(schema2, true)
        schema1.isSubsetOf(schema2).shouldBe(incompatibilities.isEmpty)
    }
  }

  it should "track the different schema types properties" in {
    val typesProp = productSchema1
      .merge(schema2)
      .asInstanceOf[ProductSchema]
      .properties
      .get[ProductSchemaTypesProperty]
    val types =
      typesProp.schemaTypes.map(_.schemaType).zip(typesProp.schemaCounts)
    types should contain theSameElementsAs List(("boolean", 1), ("integer", 2))
  }

  it should "merge all types in multiple ProductSchemas" in {
    val typesProp = ProductSchema(schema3)
      .merge(productSchema1)
      .asInstanceOf[ProductSchema]
      .properties
      .get[ProductSchemaTypesProperty]
    val types = typesProp.schemaTypes.zip(typesProp.schemaCounts)
    types should contain theSameElementsAs List(
      (schema1, 1),
      (schema2, 1),
      (schema3, 1)
    )
  }

  it should "not merge all types in multiple ProductSchemas with all" in {
    val mergedSchemaProps =
      allProductSchema
        .merge(allProductSchema)
        .asInstanceOf[ProductSchema]
        .properties
        .get[ProductSchemaTypesProperty]

    mergedSchemaProps.schemaTypes should contain theSameElementsAs List(
      StringSchema(),
      StringSchema()
    )
    mergedSchemaProps.productType shouldBe AllOf
  }

  it should "produce an AnySchema with a Union merge" in {
    productSchema1.merge(AnySchema()) shouldBe an[AnySchema]
  }

  it should "produce itself when Intersect merged with AnySchema" in {
    productSchema1.merge(AnySchema(), Intersect) shouldEqual productSchema1
  }

  it should "produce a ZeroSchema with an Intersect merge" in {
    productSchema1.merge(ZeroSchema(), Intersect) shouldBe an[ZeroSchema]
  }

  it should "produce itself when Union merged with ZeroSchema" in {
    productSchema1.merge(ZeroSchema()) shouldEqual productSchema1
  }

  it should "merge integer and number schemas together" in {
    val mergedSchemaProps = ProductSchema(schema2)
      .merge(NumberSchema())
      .asInstanceOf[ProductSchema]
      .properties
      .get[ProductSchemaTypesProperty]

    mergedSchemaProps.schemaTypes.map(
      _.schemaType
    ) should contain theSameElementsAs List("number")
  }

  it should "not created nested ProductSchemas with mixed all values" in {
    val mergedSchemaProps =
      allProductSchema
        .merge(productSchema1)
        .asInstanceOf[ProductSchema]
        .properties
        .get[ProductSchemaTypesProperty]

    mergedSchemaProps.schemaTypes should contain theSameElementsAs List(
      StringSchema(),
      productSchema1
    )
    mergedSchemaProps.productType shouldBe AllOf
  }

  it should "convert to JSON using oneOf" in {
    val anyTypes = (productSchema1.toJson() \ "oneOf").extract[JArray]
    (
      (anyTypes \ "type")(0).extract[String],
      (anyTypes \ "type")(1).extract[String]
    ) shouldBe ("boolean", "integer")
  }

  it should "allow replacement of a schema with a reference" in {
    val productSchema = ProductSchema(
      ObjectSchema(Map("foo" -> BooleanSchema()))
    ).merge(BooleanSchema()).replaceWithReference("/0/foo", "foo")
    (((productSchema.toJson() \ "oneOf")(0)) \ "properties" \ "foo")
      .extract[Map[String, String]] shouldEqual Map("$ref" -> "foo")
  }

  it should "transform types" in {
    val transformedSchema = productSchema1.transformProperties { case _ =>
      NullSchema()
    }
    transformedSchema
      .asInstanceOf[ProductSchema]
      .properties
      .get[ProductSchemaTypesProperty]
      .schemaTypes
      .shouldEqual(List(NullSchema(), NullSchema()))
  }

  it should "detect anomalies of incorrect type with oneOf" in {
    productSchema1.isAnomalous(JString("foo")).shouldBe(true)
  }

  def stringSchemaWithMinLength(minLength: Int): StringSchema = {
    val props = SchemaProperties.empty[String]
    props.add(MinLengthProperty(Some(minLength)))

    // This is here to ensure Info level anomalies do not result
    // in the product schema failing to find a match
    props.add(StringBloomFilterProperty())

    StringSchema(props)
  }

  def anomalyDetection(
      productType: ProductType,
      matchCount: Int,
      anomalous: Boolean
  ): Unit = {
    it should s"${if (anomalous) ""
      else "not "}find anomalies for a ProductSchema using ${productType} and ${matchCount} ${if (
        matchCount == 1
      )
        "match"
      else "matches"}" in {
      val schemas =
        List(stringSchemaWithMinLength(1), stringSchemaWithMinLength(2))

      // This is here to test the logic in `collectAnomalies`
      // which considers non-null values as anomalous, but
      // we need to exclude it for `AllOf`
      val schemasWithNull = if (productType != AllOf) {
        schemas ++ List(NullSchema())
      } else {
        schemas
      }

      val typesProp =
        ProductSchemaTypesProperty(
          AnySchema(),
          schemasWithNull,
          List(1),
          productType
        )
      val schema = ProductSchema(
        SchemaProperties
          .empty[JsonSchema[_]]
          .replaceProperty(typesProp)
      )
      val value: JString = JString("a" * matchCount)
      schema.isAnomalous(value) shouldBe anomalous
    }
  }

  anomalyDetection(OneOf, 0, true)
  anomalyDetection(OneOf, 1, false)
  anomalyDetection(OneOf, 2, true)
  anomalyDetection(AllOf, 1, true)
  anomalyDetection(AllOf, 2, false)
  anomalyDetection(AnyOf, 0, true)
  anomalyDetection(AnyOf, 1, false)
  anomalyDetection(AnyOf, 2, false)

  it should "detect anomalies of incorrect type" in {
    productSchema1.isAnomalous(JString("foo")).shouldBe(true)
  }

  it should "be compatible with one of the contained schemas" in {
    schema1.isSubsetOf(productSchema1) shouldBe true
    productSchema1.isSupersetOf(schema1) shouldBe true
  }

  it should "not be compatible with a schema not contained inside" in {
    productSchema1.isSubsetOf(NumberSchema(3.0)) shouldBe false
  }

  it should "can expand to cover a new type" in {
    productSchema1
      .expandTo(Some(StringSchema()))
      .asInstanceOf[ProductSchema]
      .properties
      .get[ProductSchemaTypesProperty]
      .schemaTypes
      .map(_.schemaType) should contain("string")
  }

  it should "can expand to cover object types" in {
    productSchema1
      .expandTo(Some(ObjectSchema(Map("foo" -> BooleanSchema()))))
      .asInstanceOf[ProductSchema]
      .properties
      .get[ProductSchemaTypesProperty]
      .schemaTypes
      .find(_.isInstanceOf[ObjectSchema])
      .get
      .asInstanceOf[ObjectSchema]
      .properties
      .get[ObjectTypesProperty]
      .objectTypes
      .keySet should contain("foo")
  }

  it should "expand an existing type to be compatbile" in withParams(propSet =
    PropertySets.SimpleProperties
  ) { implicit params =>
    // We need to rebuild some types using Simple properties
    // to ensure that there is something that will be in conflict
    // if the resulting types are not compatible
    val oldSchema = IntegerSchema(0)(params)
    val newSchema = IntegerSchema(3)(params)
    val productSchema1 =
      ProductSchema(schema1)(params)
        .merge(oldSchema)(params)
        .asInstanceOf[ProductSchema]

    newSchema.isSubsetOf(
      productSchema1
        .expandTo(Some(newSchema))
        .asInstanceOf[ProductSchema]
        .properties
        .get[ProductSchemaTypesProperty]
        .schemaTypes
        .find(_.schemaType == "integer")
        .get
    )(params) shouldBe true
  }

  it should "calculate entropy for a simple product" in {
    productSchema1.entropy.value shouldBe 2
  }
}

package edu.rit.cs.dataunitylab.jsonoid.discovery
package schemas

import org.json4s.{DefaultFormats, Formats}
import org.json4s.JsonDSL._
import org.json4s._

import PropertySets._
import UnitSpec.containingNatureOfSchemaProperties

class ObjectSchemaSpec extends UnitSpec {
  implicit val formats: Formats = DefaultFormats

  private val objectTypes =
    Map(
      "foo" -> BooleanSchema(),
      "bar" -> BooleanSchema(),
      "baz" ->
        BooleanSchema()
    )
  private val singleType = Map("foo" -> BooleanSchema())
  private val schemaProperties =
    ObjectSchema(singleType).properties.mergeValue(objectTypes)
  private val objectSchema = ObjectSchema(schemaProperties)

  behavior of "ObjectTypesProperty"

  it should "calculate the intersection of properties" in {
    val intersectProps = ObjectTypesProperty(singleType).intersectMerge(
      ObjectTypesProperty(objectTypes)
    )
    intersectProps.objectTypes shouldEqual Map("foo" -> BooleanSchema())
  }

  it should "track property schemas" in {
    schemaProperties should contain(ObjectTypesProperty(objectTypes))
  }

  behavior of "RequiredProperty"

  it should "track required properties" in {
    schemaProperties should contain(RequiredProperty(Some(Set("foo"))))
  }

  behavior of "FieldPresenceProperty"

  it should "track the percentage of objects with each field" in {
    val fieldPresenceProp = schemaProperties.get[FieldPresenceProperty]
    val expectedJson: JObject =
      ("fieldPresence" -> (("foo" -> JDouble(1)) ~ ("bar" -> JDouble(0.5)) ~
        ("baz" -> JDouble(0.5))))
    fieldPresenceProp.toJson() shouldEqual expectedJson
  }

  behavior of "DependenciesProperty"

  it should "track dependencies in field occurrence" in {
    val dependentSchema = ObjectSchema(
      Map(
        "foo" -> BooleanSchema(),
        "bar" ->
          BooleanSchema()
      )
    ).properties
      .mergeValue(
        Map(
          "baz" ->
            BooleanSchema()
        )
      )
      .mergeValue(Map("foo" -> BooleanSchema()))
    val dependenciesProp = dependentSchema.get[DependenciesProperty]
    (dependenciesProp.toJson() \ "dependentRequired")
      .extract[Map[String, List[String]]] shouldEqual Map("bar" -> List("foo"))
  }

  it should "be able to find disjoint subsets" in {
    val dependentSchema = ObjectSchema(
      Map(
        "foo" -> BooleanSchema(),
        "bar" -> BooleanSchema()
      )
    ).properties
      .mergeValue(
        Map(
          "baz" -> BooleanSchema(),
          "quux" -> BooleanSchema()
        )
      )
    val dependenciesProp = dependentSchema.get[DependenciesProperty]
    (dependenciesProp.disjointSets) should contain theSameElementsAs (
      Seq(Set("foo", "bar"), Set("baz", "quux"))
    )
  }

  behavior of "ObjectSchema"

  it should "be able to find subschemas by pointer" in {
    objectSchema.findByPointer("/foo") shouldBe Some(BooleanSchema())
  }

  it should "be able to find nested subschemas by pointer" in {
    val nestedSchema = ObjectSchema(Map("baz" -> objectSchema))
    nestedSchema.findByPointer("/baz/foo") shouldBe Some(BooleanSchema())
  }

  it should "have no properties in the minimal property set" in {
    val cp = new Checkpoint()

    val objectProperties = ObjectSchema(Map("foo" -> BooleanSchema()))(
      PropertySets.MinProperties,
      JsonoidParams()
    ).properties

    cp { objectProperties should have size 2 }
    cp { objectProperties.get[AdditionalPropertiesProperty] }
    cp { objectProperties.get[ObjectTypesProperty] }

    cp.reportAll()
  }

  it should "allow use of definitions" in {
    val definitionSchema = BooleanSchema()
    val objectSchema = ObjectSchema()
    objectSchema.addDefinition(definitionSchema, "foo")

    (objectSchema.toJson() \ "$defs" \ "foo") shouldEqual definitionSchema
      .toJson()
  }

  it should "allow replacement of a schema with a reference" in {
    val objectSchema = ObjectSchema(Map("foo" -> BooleanSchema()))
      .replaceWithReference("/foo", "foo")
    (objectSchema.toJson() \ "properties" \ "foo")
      .extract[Map[String, String]] shouldEqual Map("$ref" -> "foo")
  }

  it should "allow replacement of a nested schema with a reference" in {
    val objectSchema = ObjectSchema(
      Map("foo" -> ObjectSchema(Map("bar" -> BooleanSchema())))
    ).replaceWithReference("/foo/bar", "bar")
    (objectSchema.toJson() \ "properties" \ "foo" \ "properties" \ "bar")
      .extract[Map[String, String]] shouldEqual Map("$ref" -> "bar")
  }

  it should "not detect anomalies for valid values" in {
    objectSchema
      .isAnomalous(JObject(List(("foo", JBool(true)))))
      .shouldBe(false)
  }

  it should "not detect anomalies for non-object values" in {
    objectSchema.properties.flatMap(
      _.collectAnomalies(JString("foo"))
    ) shouldBe empty
  }

  it should "detect anomalies of missing required properties" in {
    schemaProperties
      .get[RequiredProperty]
      .collectAnomalies(JObject(List(("bar", JBool(true))))) shouldBe Seq(
      Anomaly("$.foo", "missing required field", AnomalyLevel.Fatal)
    )
  }

  it should "detect anomalies of extra properties with additionalProperties=false" in {
    schemaProperties
      .get[ObjectTypesProperty]
      .collectAnomalies(
        JObject(List(("foo", JBool(false)), ("quux", JBool(true))))
      ) shouldBe Seq(
      Anomaly("$.quux", "found unknown field", AnomalyLevel.Fatal)
    )
  }

  it should "not detect anomalies of extra properties with additionalProperties=true" in {
    schemaProperties
      .get[ObjectTypesProperty]
      .collectAnomalies(
        JObject(List(("foo", JBool(false)), ("quux", JBool(true))))
      ) shouldBe Seq(
      Anomaly("$.quux", "found unknown field", AnomalyLevel.Fatal)
    )
  }

  it should "detect anomalies of missing required dependencies" in {
    implicit val params =
      JsonoidParams().withAdditionalProperties(true)
    val objectSchema = ObjectSchema(Map("foo" -> BooleanSchema()))(
      PropertySets.MinProperties(params),
      params
    )
    objectSchema.properties
      .get[ObjectTypesProperty]
      .collectAnomalies(
        JObject(List(("foo", JBool(false)), ("quux", JBool(true))))
      ) shouldBe empty
  }

  it should "not allow additional properties by default" in {
    (objectSchema.toJson() \ "additionalProperties")
      .extract[Boolean] shouldBe false
  }

  it should "allow additional properties if requested" in {
    val params =
      JsonoidParams().withAdditionalProperties(true)
    val objectSchema = ObjectSchema(Map("foo" -> BooleanSchema()))(
      PropertySets.MinProperties(params),
      params
    )
    (objectSchema.toJson() \ "additionalProperties")
      .extract[Boolean] shouldBe true
  }

  it should "show matching objects as compatible" in {
    objectSchema.isCompatibleWith(objectSchema) shouldBe true
  }

  it should "show an object with a subset of optional properties as compatible" in {
    objectSchema.isCompatibleWith(ObjectSchema(singleType)) shouldBe true
  }

  it should "show an object with a superset of properties as compatible if additionalProperties is true" in {
    val p: JsonoidParams =
      JsonoidParams.defaultJsonoidParams.withAdditionalProperties(true)
    ObjectSchema(singleType)(PropertySets.AllProperties, p).isCompatibleWith(
      objectSchema
    )(p) shouldBe true
  }

  it should "show an object with a superset of properties as compatible if additionalProperties is false" in {
    ObjectSchema(singleType).isCompatibleWith(objectSchema) shouldBe false
  }

  it should "expand to add new keys" in {
    val schema =
      ObjectSchema(Map("foo" -> BooleanSchema(), "bar" -> BooleanSchema()))
    objectSchema.expandTo(Some(schema)).isCompatibleWith(schema) shouldBe true
  }

  it should "expand to combine types in the same key" in {
    val schema1 = ObjectSchema(Map("foo" -> IntegerSchema(1)))
    val schema2 = ObjectSchema(Map("foo" -> IntegerSchema(2)))
    schema1.expandTo(Some(schema2)).isCompatibleWith(schema2) shouldBe true
  }

  it should "expand to add additionalProperties" in {
    objectSchema.properties
      .get[AdditionalPropertiesProperty]
      .additionalProperties shouldBe false
    objectSchema
      .expandTo(None)
      .asInstanceOf[ObjectSchema]
      .properties
      .get[AdditionalPropertiesProperty]
      .additionalProperties shouldBe true
  }
}

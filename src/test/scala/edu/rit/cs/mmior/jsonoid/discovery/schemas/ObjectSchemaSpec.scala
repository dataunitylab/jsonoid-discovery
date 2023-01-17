package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s.{DefaultFormats, Formats}
import org.json4s.JsonDSL._
import org.json4s._

import PropertySets._
import UnitSpec._

class ObjectSchemaSpec extends UnitSpec {
  behavior of "ObjectSchema"

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

  it should "calculate the intersection of properties" in {
    val intersectProps = ObjectTypesProperty(singleType).intersectMerge(
      ObjectTypesProperty(objectTypes)
    )
    intersectProps.objectTypes shouldEqual Map("foo" -> BooleanSchema())
  }

  it should "track required properties" in {
    schemaProperties should contain(RequiredProperty(Some(Set("foo"))))
  }

  it should "track property schemas" in {
    schemaProperties should contain(ObjectTypesProperty(objectTypes))
  }

  it should "track the percentage of objects with each field" in {
    val fieldPresenceProp = schemaProperties.get[FieldPresenceProperty]
    val expectedJson: JObject =
      ("fieldPresence" -> (("foo" -> JDouble(1)) ~ ("bar" -> JDouble(0.5)) ~
        ("baz" -> JDouble(0.5))))
    fieldPresenceProp.toJson shouldEqual expectedJson
  }

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
    (dependenciesProp.toJson \ "dependentRequired")
      .extract[Map[String, List[String]]] shouldEqual Map("bar" -> List("foo"))
  }

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
      JsonoidParams.defaultJsonoidParams
    ).properties

    cp { objectProperties should have size 1 }
    cp { objectProperties.get[ObjectTypesProperty] }

    cp.reportAll()
  }

  it should "allow use of definitions" in {
    val definitionSchema = BooleanSchema()
    val objectSchema = ObjectSchema()
    objectSchema.addDefinition(definitionSchema, "foo")

    (objectSchema.toJson \ "$defs" \ "foo") shouldEqual definitionSchema.toJson
  }

  it should "allow replacement of a schema with a reference" in {
    val objectSchema = ObjectSchema(Map("foo" -> BooleanSchema()))
      .replaceWithReference("/foo", "foo")
    (objectSchema.toJson \ "properties" \ "foo")
      .extract[Map[String, String]] shouldEqual Map("$ref" -> "foo")
  }

  it should "allow replacement of a nested schema with a reference" in {
    val objectSchema = ObjectSchema(
      Map("foo" -> ObjectSchema(Map("bar" -> BooleanSchema())))
    ).replaceWithReference("/foo/bar", "bar")
    (objectSchema.toJson \ "properties" \ "foo" \ "properties" \ "bar")
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
      Anomaly("$.foo", "missing required field", Fatal)
    )
  }

  it should "detect anomalies of missing required dependencies" in {
    schemaProperties
      .get[DependenciesProperty]
      .collectAnomalies(JObject(List(("bar", JBool(true))))) shouldBe Seq(
      Anomaly("$", "dependency $.baz not found for $.bar", Fatal)
    )
  }
}

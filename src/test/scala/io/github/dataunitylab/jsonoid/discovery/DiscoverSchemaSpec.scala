package io.github.dataunitylab.jsonoid.discovery
package schemas

import scala.io.Source

import com.networknt.schema.{JsonSchemaFactory, SpecVersion}
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class DiscoverSchemaSpec extends UnitSpec with ScalaCheckPropertyChecks {
  behavior of "DiscoverSchema"

  it should "generate schemas from a value such that the value is not anomalous" in {
    forAll(JsonGen.genObject) { value =>
      val schema = DiscoverSchema.discoverFromValue(value).get
      schema.collectAnomalies(value) should be(empty)
    }
  }

  it should "produce a product schema" in {
    val schema =
      DiscoverSchema.discover(Seq(JBool(true), JString("foo")).iterator)
    DiscoverSchema.discover(
      Seq(JBool(true), JString("foo")).iterator
    ) shouldBe a[ProductSchema]
  }

  it should "produce an array schema" in {
    DiscoverSchema
      .discoverFromValue(JArray(List(JBool(true))))
      .get shouldBe a[ArraySchema]
  }

  it should "produce an array schema from a set" in {
    DiscoverSchema
      .discoverFromValue(JSet(Set(JBool(true))))
      .get shouldBe a[ArraySchema]
  }

  it should "produce a boolean schema" in {
    DiscoverSchema.discoverFromValue(JBool(true)).get shouldBe a[BooleanSchema]
  }

  it should "produce a number schema" in {
    DiscoverSchema.discoverFromValue(JDecimal(1.0)).get shouldBe a[NumberSchema]
  }

  it should "produce an integer schema" in {
    DiscoverSchema.discoverFromValue(JInt(1)).get shouldBe a[IntegerSchema]
  }

  it should "produce an integer schema from a long" in {
    DiscoverSchema.discoverFromValue(JLong(1)).get shouldBe a[IntegerSchema]
  }

  it should "produce a null schema" in {
    DiscoverSchema.discoverFromValue(JNull).get shouldBe a[NullSchema]
  }

  it should "produce a null schema from nothing" in {
    DiscoverSchema.discoverFromValue(JNothing).get shouldBe a[NullSchema]
  }

  it should "produce an object schema" in {
    DiscoverSchema
      .discoverFromValue(
        JObject(List(("foo", JBool(true))))
      )
      .get shouldBe a[ObjectSchema]
  }

  it should "produce a string schema" in {
    DiscoverSchema
      .discoverFromValue(JString("foo"))
      .get shouldBe a[StringSchema]
  }

  it should "produce nothing for Infinity" in {
    DiscoverSchema.discoverFromValue(
      JDouble(Double.PositiveInfinity)
    ) shouldBe None
  }

  it should "produce minimal properties when requested" in withParams(propSet =
    PropertySets.MinProperties
  ) { implicit params =>
    val stringSchema =
      DiscoverSchema.discover(Seq(JString("foo")).iterator)(params)
    stringSchema.properties shouldBe empty
  }

  it should "produce a valid schema for given documents" in {
    val files = Table(
      "filename",
      "earthquakes.json",
      "gdp.json",
      "mr-robot.json",
      "nobel.json",
      "rickandmorty.json",
      "test.json",
      "jsonlines-example.json"
    )

    forAll(files) { filename: String =>
      val url = getClass.getResource("/" + filename)
      val input = DiscoverSchema.jsonFromSource(Source.fromURL(url)).buffered
      val firstDoc = input.head
      val schema = DiscoverSchema.discover(input)(
        JsonoidParams().withPropertySet(PropertySets.SimpleProperties)
      )

      // XXX This validation is not perfect, but we'll check later with AJV
      val factory =
        JsonSchemaFactory.getInstance(SpecVersion.VersionFlag.V202012)
      val jsonSchema = factory.getSchema(asJsonNode(schema.toJson()))
      val errors = jsonSchema.validate(asJsonNode(firstDoc))
      errors shouldBe empty
    }
  }

  it should "reset min/max length for strings when transforming" in {
    implicit val propSet = PropertySets.SimpleProperties
    var schema = StringSchema("http://example.com/")
    val p = JsonoidParams.defaultJsonoidParams.withResetFormatLength(true)
    for (_ <- 1 to 10) {
      schema = schema
        .mergeSameType(Union)(p)(schema)
        .asInstanceOf[StringSchema]
    }
    val transformedSchema =
      DiscoverSchema.transformSchema(schema)(p).asInstanceOf[StringSchema]
    transformedSchema.properties.get[MinLengthProperty].minLength shouldBe None
    transformedSchema.properties.get[MaxLengthProperty].maxLength shouldBe None
  }
}

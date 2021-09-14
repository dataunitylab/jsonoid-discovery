package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import java.io.PrintWriter
import java.nio.file.{FileSystems,Files}
import scala.io.Source

import com.networknt.schema.{JsonSchemaFactory, SpecVersion}
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.scalatest.prop.TableDrivenPropertyChecks._


class DiscoverSchemaSpec extends UnitSpec {
  behavior of "DiscoverSchema"

  it should "produce a product schema" in {
    val schema = DiscoverSchema.discover(Seq(JBool(true), JString("foo")).iterator)
    DiscoverSchema.discover(Seq(JBool(true), JString("foo")).iterator) shouldBe a [ProductSchema]
  }

  it should "produce an array schema" in {
    DiscoverSchema.discoverFromValue(JArray(List(JBool(true)))) shouldBe a [ArraySchema]
  }

  it should "produce an array schema from a set" in {
    DiscoverSchema.discoverFromValue(JSet(Set(JBool(true)))) shouldBe a [ArraySchema]
  }

  it should "produce a boolean schema" in {
    DiscoverSchema.discoverFromValue(JBool(true)) shouldBe a [BooleanSchema]
  }

  it should "produce a number schema" in {
    DiscoverSchema.discoverFromValue(JDecimal(1.0)) shouldBe a [NumberSchema]
  }

  it should "produce an integer schema" in {
    DiscoverSchema.discoverFromValue(JInt(1)) shouldBe a [IntegerSchema]
  }

  it should "produce an integer schema from a long" in {
    DiscoverSchema.discoverFromValue(JLong(1)) shouldBe a [IntegerSchema]
  }

  it should "produce a null schema" in {
    DiscoverSchema.discoverFromValue(JNull) shouldBe a [NullSchema]
  }

  it should "produce a null schema from nothing" in {
    DiscoverSchema.discoverFromValue(JNothing) shouldBe a [NullSchema]
  }

  it should "produce an object schema" in {
    DiscoverSchema.discoverFromValue(JObject(List(("foo", JBool(true))))) shouldBe a [ObjectSchema]
  }

  it should "produce a string schema" in {
    DiscoverSchema.discoverFromValue(JString("foo")) shouldBe a [StringSchema]
  }

  it should "produce minimal properties when requested" in {
    val stringSchema =DiscoverSchema.discover(
      Seq(JString("foo")).iterator,
      PropertySets.MinProperties
    )
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

    // Generate the output directory to store generated schemas
    val schemaPath = FileSystems.getDefault().getPath("target", "jsonoid-schemas")
    Files.createDirectories(schemaPath)

    forAll(files) { filename: String =>
      val url = getClass.getResource("/" + filename)
      val input = DiscoverSchema.jsonFromSource(Source.fromURL(url)).buffered
      val firstDoc = input.head
      val schema = DiscoverSchema.discover(input, PropertySets.SimpleProperties)

      // Save a copy of the generated schema
      new PrintWriter(schemaPath.resolve(filename).toFile) {
        write(compact(render(schema.toJson)))
        close
      }

      // XXX This validation is not perfect, but we'll check later with AJV
      //     Specifically, version 2020-12 of the spec is not yet supported
      val factory = JsonSchemaFactory.getInstance(SpecVersion.VersionFlag.V201909)
      val jsonSchema = factory.getSchema(asJsonNode(schema.toJson))
      val errors = jsonSchema.validate(asJsonNode(firstDoc))
      errors shouldBe empty
    }
  }
}

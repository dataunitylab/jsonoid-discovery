package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import scala.io.Source

import com.networknt.schema.{JsonSchemaFactory, SpecVersion}
import org.json4s.jackson.JsonMethods._


class ValidationSpec extends UnitSpec {
  behavior of "DiscoverSchema"

  it should "produce a valid schema for the given documents" in {
    val url = getClass.getResource("/test.json")
    val input = DiscoverSchema.jsonFromSource(Source.fromURL(url))
    val schema = DiscoverSchema.discover(input)

    val factory = JsonSchemaFactory.getInstance(SpecVersion.VersionFlag.V4)
    val jsonSchema = factory.getSchema(asJsonNode(schema.toJson))

    val errors = jsonSchema.validate(asJsonNode(input(0)))
    errors shouldBe empty
  }
}

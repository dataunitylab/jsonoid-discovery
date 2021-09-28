package edu.rit.cs.mmior.jsonoid.discovery

import schemas._
import PropertySets._

import java.io.ByteArrayOutputStream

class ValueTableGeneratorSpec extends UnitSpec {
  behavior of "ValueTableGenerator"

  it should "produce a table of values from examples" in {
    val objectSchema = ObjectSchema(Map(
      "foo" -> StringSchema("bar"),
      "baz" -> ArraySchema(List(StringSchema("quux"), StringSchema("corge")))
    ))

    val output = new ByteArrayOutputStream()
    ValueTableGenerator.writeValueTable(objectSchema, output)

    output.toString("UTF-8") should equal ("$.baz[0],$.baz[1],$.foo\r\nquux,corge,bar\r\n")
  }
}

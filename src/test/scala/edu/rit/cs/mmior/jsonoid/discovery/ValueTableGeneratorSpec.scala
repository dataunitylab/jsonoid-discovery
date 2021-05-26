package edu.rit.cs.mmior.jsonoid.discovery

import schemas._

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

    output.toString("UTF-8") should equal ("$.baz[*],$.foo\r\ncorge,bar\r\nquux,\r\n")
  }
}

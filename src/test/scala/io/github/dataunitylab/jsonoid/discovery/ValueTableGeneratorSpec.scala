package io.github.dataunitylab.jsonoid.discovery

import schemas._

import java.io.ByteArrayOutputStream

class ValueTableGeneratorSpec extends UnitSpec {
  behavior of "ValueTableGenerator"

  it should "produce a table of values from examples" in {
    val objectSchema = ObjectSchema(
      Map(
        "foo" -> StringSchema("bar"),
        "baz" -> ArraySchema(List(StringSchema("quux"), StringSchema("corge"))),
        "grault" -> IntegerSchema(1),
        "foobar" -> NumberSchema(3.14)
      )
    )

    val output = new ByteArrayOutputStream()
    ValueTableGenerator.writeValueTable(objectSchema, output)

    output.toString("UTF-8") should equal(
      "$.baz[0],$.baz[1],$.foo,$.foobar,$.grault\r\nquux,corge,bar,3.14,1\r\n"
    )
  }
}

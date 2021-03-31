package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class DiscoverSchemaSpec extends AnyFlatSpec with Matchers {
  behavior of "DiscoverSchema"

  it should "produce a boolean schema" in {
    DiscoverSchema.discoverFromValue(JBool(true)) shouldEqual BooleanSchema()
  }

  it should "produce a number schema" in {
    DiscoverSchema.discoverFromValue(JDecimal(1.0)) shouldEqual NumberSchema(SchemaProperties(Seq(MinNumValueProperty(Some(1.0)), MaxNumValueProperty(Some(1.0)))))
  }

  it should "produce an integer schema" in {
    DiscoverSchema.discoverFromValue(JInt(1)) shouldEqual IntegerSchema(SchemaProperties(Seq(MinIntValueProperty(Some(1)), MaxIntValueProperty(Some(1)))))
  }

  it should "produce a null schema" in {
    DiscoverSchema.discoverFromValue(JNull) shouldEqual NullSchema()
  }

  it should "produce a string schema" in {
    DiscoverSchema.discoverFromValue(JString("foo")) shouldEqual StringSchema(SchemaProperties(Seq(MinLengthProperty(Some(3)), MaxLengthProperty(Some(3)))))
  }
}

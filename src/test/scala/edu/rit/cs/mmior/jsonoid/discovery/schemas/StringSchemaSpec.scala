package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import UnitSpec._

class StringSchemaSpec extends UnitSpec {
  behavior of "StringSchema"

  private val stringSchema = StringSchema("foo").properties.merge("foobar")

  it should "track the maximum length" in {
    stringSchema should contain (MaxLengthProperty(Some(6)))
  }

  it should "track the minimum length" in {
    stringSchema should contain (MinLengthProperty(Some(3)))
  }
}

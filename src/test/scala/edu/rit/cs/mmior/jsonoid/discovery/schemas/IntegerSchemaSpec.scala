package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import UnitSpec._

class IntegerSchemaSpec extends UnitSpec {
  behavior of "IntegerSchema"

  private val integerSchema = IntegerSchema(3).properties.merge(4)

  it should "track the maximum length" in {
    integerSchema should contain (MaxIntValueProperty(Some(4)))
  }

  it should "track the minimum length" in {
    integerSchema should contain (MinIntValueProperty(Some(3)))
  }
}

package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import UnitSpec._

class NumberSchemaSpec extends UnitSpec {
  behavior of "NumberSchema"

  private val numberSchema = NumberSchema(3).properties.merge(4)

  it should "track the maximum length" in {
    numberSchema should contain (MaxNumValueProperty(Some(4)))
  }

  it should "track the minimum length" in {
    numberSchema should contain (MinNumValueProperty(Some(3)))
  }
}

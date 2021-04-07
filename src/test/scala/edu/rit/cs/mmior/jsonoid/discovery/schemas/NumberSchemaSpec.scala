package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import UnitSpec._

class NumberSchemaSpec extends UnitSpec {
  behavior of "NumberSchema"

  private val numberSchema = NumberSchema(3.14).properties.merge(4.28)

  it should "track the maximum length" in {
    numberSchema should contain (MaxNumValueProperty(Some(4.28)))
  }

  it should "track the minimum length" in {
    numberSchema should contain (MinNumValueProperty(Some(3.14)))
  }

  it should "track the distinct elements" in {
    val hyperLogLogProp = numberSchema.find(_.isInstanceOf[NumHyperLogLogProperty]).fold(NumHyperLogLogProperty())(_.asInstanceOf[NumHyperLogLogProperty])
    hyperLogLogProp.hll.count() should be (2)
  }
}

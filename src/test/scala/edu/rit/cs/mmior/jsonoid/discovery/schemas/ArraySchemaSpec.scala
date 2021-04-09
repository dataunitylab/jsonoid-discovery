package edu.rit.cs.mmior.jsonoid.discovery
package schemas

import UnitSpec._

class ArraySchemaSpec extends UnitSpec {
  behavior of "ArraySchema"

  private val itemType = BooleanSchema()
  private val arraySchema = ArraySchema(List(itemType)).properties.mergeValue(List(itemType, itemType))

  it should "track item schemas" in {
    arraySchema should contain (ItemTypeProperty(itemType))
  }

  it should "track minimum array length" in {
    arraySchema should contain (MinArrayLengthProperty(Some(1)))
  }

  it should "track maximum array length" in {
    arraySchema should contain (MaxArrayLengthProperty(Some(2)))
  }
}

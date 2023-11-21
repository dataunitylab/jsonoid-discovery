package io.github.dataunitylab.jsonoid.discovery
package schemas

import UnitSpec._

class SchemaPropertiesSpec extends UnitSpec {
  behavior of "SchemaProperties"

  it should "merge two property sets" in {
    val prop1 = SchemaProperties.empty[BigInt]
    prop1.add(MinIntValueProperty())

    val prop2 = SchemaProperties.empty[BigInt]
    prop2.add(MaxIntValueProperty())

    val merged = prop1.merge(prop2)
    (merged.has[MinIntValueProperty]) shouldBe true
    (merged.has[MaxIntValueProperty]) shouldBe true
  }

  it should "return None if a property does not exist" in {
    val props = SchemaProperties.empty[BigInt]
    props.getOrNone[MinIntValueProperty] shouldBe None
  }
}

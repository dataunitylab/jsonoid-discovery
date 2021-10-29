package edu.rit.cs.mmior.jsonoid.discovery
package schemas

class SchemaPropertiesSpec extends UnitSpec {
  behavior of "SchemaProperties"

  implicit val er: EquivalenceRelation =
    EquivalenceRelations.KindEquivalenceRelation

  it should "merge two property sets" in {
    val prop1 = SchemaProperties.empty[BigInt]
    prop1.add(MinIntValueProperty())

    val prop2 = SchemaProperties.empty[BigInt]
    prop2.add(MaxIntValueProperty())

    val merged = prop1.merge(prop2)
    (merged.has[MinIntValueProperty]) shouldBe true
    (merged.has[MaxIntValueProperty]) shouldBe true
  }
}

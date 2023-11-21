package io.github.dataunitylab.jsonoid.discovery
package schemas

class PropertySetsSpec extends UnitSpec {
  behavior of "SchemaProperties"

  it should "be able to include only properties which match a given name" in {
    val propSet =
      PropertySets.AllProperties.onlyNamed(List("MinLengthProperty"))
    propSet.arrayProperties shouldBe empty
    propSet.booleanProperties shouldBe empty
    propSet.integerProperties shouldBe empty
    propSet.numberProperties shouldBe empty
    propSet.objectProperties shouldBe empty
    propSet.stringProperties.properties.keySet
      .map(_.toString) should contain theSameElementsAs List(
      "io.github.dataunitylab.jsonoid.discovery.schemas.MinLengthProperty"
    )
  }

  it should "be able to include only properties which match a given class" in {
    val propSet =
      PropertySets.AllProperties.only(List(classOf[MinLengthProperty]))
    propSet.arrayProperties shouldBe empty
    propSet.booleanProperties shouldBe empty
    propSet.integerProperties shouldBe empty
    propSet.numberProperties shouldBe empty
    propSet.objectProperties shouldBe empty
    propSet.stringProperties.properties.keySet
      .map(_.toString) should contain theSameElementsAs List(
      "io.github.dataunitylab.jsonoid.discovery.schemas.MinLengthProperty"
    )
  }

  it should "be able to exclude only properties which don't match a given name" in {
    val propSet =
      PropertySets.AllProperties.withoutNamed(List("MinLengthProperty"))
    propSet.stringProperties.properties.keySet
      .map(
        _.toString
      ) should not contain ("io.github.dataunitylab.jsonoid.discovery.schemas.MinLengthProperty")
  }

  it should "be able to exclude only properties which don't match a given class" in {
    val propSet =
      PropertySets.AllProperties.without(List(classOf[MinLengthProperty]))
    propSet.stringProperties.properties.keySet
      .map(
        _.toString
      ) should not contain ("io.github.dataunitylab.jsonoid.discovery.schemas.MinLengthProperty")
  }
}

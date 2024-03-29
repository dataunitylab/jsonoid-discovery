package io.github.dataunitylab.jsonoid.discovery

import scalaz._
import Scalaz._

import Helpers._

class HelpersSpec extends UnitSpec {
  val none: Option[String] = None

  behavior of "isMinCoveredBy"

  it should "be covered for an equal minimum" in {
    isMinCoveredBy(Some(3.5), false, Some(3.5), false) shouldBe true
  }

  it should "not be covered for an equal minimum if the other is exclusive" in {
    isMinCoveredBy(Some(3.5), false, Some(3.5), true) shouldBe false
  }

  it should "be covered for an equal minimum if both are exclusive" in {
    isMinCoveredBy(Some(3.5), true, Some(3.5), true) shouldBe true
  }

  it should "be covered for a smaller minimum" in {
    isMinCoveredBy(Some(3.5), false, Some(3.0), false) shouldBe true
  }

  it should "not be covered for a larger minimum" in {
    isMinCoveredBy(Some(3.5), false, Some(4.0), false) shouldBe false
  }

  it should "no minimum cannot be covered with a fixed minimum" in {
    isMinCoveredBy(None, false, Some(3.5), false) shouldBe false
  }

  it should "be covered if no other minimum" in {
    isMinCoveredBy(Some(3.5), false, None, false) shouldBe true
  }

  it should "no minimum can covered if there is no other minimum" in {
    val noNum: Option[Double] = None
    isMinCoveredBy(noNum, false, noNum, false) shouldBe true
  }

  behavior of "isMaxCoveredBy"

  it should "be covered for an equal maximum" in {
    isMaxCoveredBy(Some(3.5), false, Some(3.5), false) shouldBe true
  }

  it should "not be covered for an equal maximum if the other is exclusive" in {
    isMaxCoveredBy(Some(3.5), false, Some(3.5), true) shouldBe false
  }

  it should "be compatible for an equal maximum if both are exclusive" in {
    isMaxCoveredBy(Some(3.5), true, Some(3.5), true) shouldBe true
  }

  it should "be covered by a larger maximum" in {
    isMaxCoveredBy(Some(3.0), false, Some(3.5), false) shouldBe true
  }

  it should "not be covered for a smaller maximum" in {
    isMaxCoveredBy(Some(4.0), false, Some(3.5), false) shouldBe false
  }

  it should "no maximum cannot be covered with a fixed maximum" in {
    isMaxCoveredBy(None, false, Some(3.5), false) shouldBe false
  }

  it should "be covered if no other maximum" in {
    isMinCoveredBy(Some(3.5), false, None, false) shouldBe true
  }

  it should "no maximum can covered if there is no other maximum" in {
    val noNum: Option[Double] = None
    isMaxCoveredBy(noNum, false, noNum, false) shouldBe true
  }

  behavior of "maxOrNone"

  it should "give the max value of two Some" in {
    maxOrNone(Some("bar"), Some("foo")) should be(Some("foo"))
  }

  it should "give a value when finding the max with None" in {
    maxOrNone(none, Some("foo")).value shouldBe "foo"
  }

  it should "give None with the max of two None values" in {
    maxOrNone(none, none) shouldBe None
  }

  behavior of "minOrNone"

  it should "give the min value of two Some" in {
    minOrNone(Some("bar"), Some("foo")) should be(Some("bar"))
  }

  it should "give a value when finding the min with None" in {
    minOrNone(none, Some("foo")).value shouldBe "foo"
  }

  it should "give None with the min of two None values" in {
    minOrNone(none, none) shouldBe None
  }

  behavior of "intersectOrNone"

  it should "give None with the intersection of two None values" in {
    intersectOrNone(None, None) should be(None)
  }

  it should "give one set with the intersection with None" in {
    intersectOrNone(Some(Set(1)), None) should be(Some(Set(1)))
  }

  it should "give the second set if intersected with None" in {
    intersectOrNone(None, Some(Set(1))) should be(Some(Set(1)))
  }

  it should "give the intersection if both sets are not None" in {
    intersectOrNone(Some(Set(1, 2)), Some(Set(0, 1))) should be(Some(Set(1)))
  }

  behavior of "maybeExpandInt"

  it should "expand to increment values by tens" in {
    maybeExpandInt(Some(101), Some(117), false) shouldBe (Some(120), false)
  }

  it should "expand to increment values by one less than powers of 2" in {
    maybeExpandInt(Some(123), Some(124), false) shouldBe (Some(127), false)
  }

  it should "expand negative values to remove the limit" in {
    maybeExpandInt(Some(-123), Some(124), false) shouldBe (None, false)
  }

  it should "not expand if already covered" in {
    maybeExpandInt(Some(14), Some(12), false) shouldBe (Some(14), false)
  }

  it should "correctly expand very large numbers" in {
    maybeExpandInt(
      Some(BigInt("1196296863810379800")),
      Some(BigInt("1196296863810379801")),
      false
    ) shouldBe (Some(BigInt("1200000000000000000")), false)
  }

  behavior of "maybeContractInt"

  it should "expand to decrement values by tens" in {
    maybeContractInt(Some(117), Some(101), false) shouldBe (Some(100), false)
  }

  it should "expand to decrement values by powers of 2" in {
    maybeContractInt(Some(130), Some(129), false) shouldBe (Some(128), false)
  }

  it should "not expand if already covered" in {
    maybeContractInt(Some(14), Some(28), false) shouldBe (Some(14), false)
  }
}

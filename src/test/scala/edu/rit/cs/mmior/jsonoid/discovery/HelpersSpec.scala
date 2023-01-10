package edu.rit.cs.mmior.jsonoid.discovery

import scalaz._
import Scalaz._

import Helpers._

class HelpersSpec extends UnitSpec {
  val none: Option[String] = None

  behavior of "maxOrNone"

  it should "give the max value of two Some" in {
    maxOrNone(Some("bar"), Some("foo")) should be(Some("foo"))
  }

  it should "give a value when finding the max with None" in {
    maxOrNone(none, Some("foo")) should be(Some("foo"))
  }

  it should "give None with the max of two None values" in {
    maxOrNone(none, none) should be(None)
  }

  behavior of "minOrNone"

  it should "give the min value of two Some" in {
    minOrNone(Some("bar"), Some("foo")) should be(Some("bar"))
  }

  it should "give a value when finding the min with None" in {
    minOrNone(none, Some("foo")) should be(Some("foo"))
  }

  it should "give None with the min of two None values" in {
    minOrNone(none, none) should be(None)
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
}

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
}

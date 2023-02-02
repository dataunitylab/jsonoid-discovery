package edu.rit.cs.mmior.jsonoid.discovery

import scala.annotation.tailrec

import scalaz._
import Scalaz._

object Helpers {
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def isMinCompatibleWith[A: Order](
      value1: Option[A],
      exclusive1: Boolean,
      value2: Option[A],
      exclusive2: Boolean
  ): Boolean = {
    if (value1.isEmpty) {
      // If we have no minimum, then compatible
      true
    } else if (value2.isEmpty) {
      // If we have a minimum and the other schema doesn't, not compatible
      false
    } else if (!exclusive2 && exclusive1) {
      // If we are exclusive and the other schema is not,
      // then the other minmum value must be greater
      value2.get > value1.get
    } else {
      // Otherwise, minimum value can be greater than or equal
      value2.get >= value1.get
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def isMaxCompatibleWith[A: Order](
      value1: Option[A],
      exclusive1: Boolean,
      value2: Option[A],
      exclusive2: Boolean
  ): Boolean = {
    if (value1.isEmpty) {
      // If we have no maximum, then compatible
      true
    } else if (value2.isEmpty) {
      // If we have a maximum and the other schema doesn't, not compatible
      false
    } else if (!exclusive2 && exclusive1) {
      // If we are exclusive and the other schema is not,
      // then the other maxmum value must be greater
      value2.get < value1.get
    } else {
      // Otherwise, maximum value can be greater than or equal
      value2.get <= value1.get
    }
  }

  def maxOrNone[A: Order](first: Option[A], second: Option[A]): Option[A] =
    (first, second) match {
      case (Some(a), None) => first
      case (None, Some(b)) => second
      case (Some(a), Some(b)) =>
        if (b > a) { second }
        else { first }
      case (None, None) => None
    }

  def minOrNone[A: Order](first: Option[A], second: Option[A]): Option[A] =
    (first, second) match {
      case (Some(a), None) => first
      case (None, Some(b)) => second
      case (Some(a), Some(b)) =>
        if (b < a) { second }
        else { first }
      case (None, None) => None
    }

  def intersectOrNone[A](
      first: Option[Set[A]],
      second: Option[Set[A]]
  ): Option[Set[A]] = (first, second) match {
    case (Some(a), None)    => first
    case (None, Some(b))    => second
    case (Some(a), Some(b)) => Some(a.intersect(b))
    case (None, None)       => None
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def findCommonPrefix(str1: String, str2: String): String = {
    (str1, str2).zipped
      .takeWhile(Function.tupled(_ == _))
      .map(_._1)
      .mkString
  }

  def findCommonPrefix(
      str1: Option[String],
      str2: Option[String]
  ): Option[String] = {
    (str1, str2) match {
      case (Some(str1), Some(str2)) => Some(findCommonPrefix(str1, str2))
      case (None, x)                => x
      case (x, None)                => x
    }
  }

  // scalastyle:off method.name
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit final class AnyOps[A](self: A) {
    def ===(other: A): Boolean = self == other

    def =/=(other: A): Boolean = self != other
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit final class BigIntOps(self: BigInt) {
    def ===(other: BigInt): Boolean = self == other
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit final class BigDecimalOps(self: BigDecimal) {
    def ===(other: BigDecimal): Boolean = self == other
  }
  // scalastyle:on method.name

  @tailrec
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a else gcd(b, a % b)

  @tailrec
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def gcd(a: BigDecimal, b: BigDecimal): BigDecimal =
    if (b == 0) a else gcd(b, a % b)

  def lcm(a: BigInt, b: BigInt): BigInt = a * b / gcd(a, b)

  def lcm(a: BigDecimal, b: BigDecimal): BigDecimal = a * b / gcd(a, b)
}

package io.github.dataunitylab.jsonoid.discovery

import scala.annotation.tailrec

import scalaz._
import Scalaz._

import utils.JsonPointer

/** Various helper functions used in the rest of the code.
  */
object Helpers {
  def pathToInexactPointer(path: String): JsonPointer = {
    // Path must not be empty
    assert(path.nonEmpty)

    // Change dots to slashses and remove any array accesses
    JsonPointer.fromString(
      path
        .substring(1)
        .replace("~", "~0")
        .replace("/", "~1")
        .replace(".", "/")
        .replaceAll("\\[[^]+]\\]", "")
    )
  }

  /** The maximum number of rounds to consider during schema expansion. */
  val MaxExpandRounds: Int = 10

  /** A wrapper for [[expandInt]] that works with [[scala.Option]] values. */
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def maybeExpandInt(
      current: Option[BigInt],
      limit: Option[BigInt],
      exclusive: Boolean,
      force: Boolean = false,
      round: Int = 1
  ): (Option[BigInt], Boolean) = {
    if (round > MaxExpandRounds) {
      (None, false)
    } else {
      if (force && current.isDefined) {
        // The current limit doesn't matter here since we're forcing
        expandInt(current.get, 0, exclusive, true, round)
      } else {
        (current, limit) match {
          case (Some(currentInt), Some(otherInt)) =>
            expandInt(currentInt, otherInt, exclusive, force, round)

          // We have no limit, keep it that way
          case (None, _) => (None, false)

          // There is no limit on the other schema, so we can't have one here
          case (_, None) => (None, false)
        }
      }
    }
  }

  /** Expands an integer used for a minimum value to meet a given limit.
    *
    * @param current
    *   the current minimum value
    * @param limit
    *   the limit from the other schema
    * @param exclusive
    *   whether the current limit is exclusive or not
    * @param round
    *   the current round of expansion
    *
    * @return
    *   the new minimum value and whether it is exclusive
    */
  def expandInt(
      current: BigInt,
      limit: BigInt,
      exclusive: Boolean,
      force: Boolean = false,
      round: Int
  ): (Option[BigInt], Boolean) = {
    // If we got here, we must not have exceeded the max number of rounds
    assert(round <= MaxExpandRounds)

    if (current < 0) {
      // TODO We can probably make better choices for negative values
      maybeExpandInt(Some(0), Some(limit), exclusive, force, round + 1)
    } else if (exclusive && (current < limit || force)) {
      maybeExpandInt(Some(current), Some(limit), false, force, round + 1)
    } else if (
      !force && ((exclusive && current > limit) || (!exclusive && current >= limit))
    ) {
      (Some(current), exclusive)
    } else if (current < 10) {
      maybeExpandInt(
        Some(current + 1),
        Some(limit),
        exclusive,
        force,
        round + 1
      )
    } else {
      // Since 2 is a common power, consider the next closest power
      // but first consider one less since this occurs frequently
      // (e.g. 255, 65535)
      // The next closest power of two is just 2^(number of binary digits)
      val closestPow2 = BigInt(2).pow(current.toString(2).length)
      val next2 = if ((closestPow2 - 1) > current) {
        closestPow2 - 1
      } else {
        closestPow2
      }

      // Otherwise, increase by the closest power of 10 (examples below)
      // 12 => 13
      // 20 => 30
      // 99 => 100
      // 110 => 200
      // 217 => 300
      // The floor of the base 10 logarithm is the number
      // of decimal digits in the number minus 1
      val pow10 = BigInt(10).pow(current.toString.length - 1 - 1)
      val next10 = current + pow10 - current.mod(pow10)
      if (next2 < next10 && next2 > current)
        maybeExpandInt(Some(next2), Some(limit), exclusive, force, round + 1)
      else
        maybeExpandInt(Some(next10), Some(limit), exclusive, force, round + 1)
    }
  }

  /** A wrapper for [[contractInt]] that works with [[scala.Option]] values.
    */
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def maybeContractInt(
      current: Option[Int],
      limit: Option[Int],
      exclusive: Boolean,
      force: Boolean = false,
      round: Int = 1
  ): (Option[Int], Boolean) = {
    if (round > MaxExpandRounds) {
      (None, false)
    } else {
      if (force && current.isDefined) {
        // The current limit doesn't matter here since we're forcing
        contractInt(current.get, 0, exclusive, true, round)
      } else {
        (current, limit) match {
          case (Some(currentInt), Some(otherInt)) =>
            contractInt(currentInt, otherInt, exclusive, force, round)

          // We have no limit, keep it that way
          case (None, _) => (None, false)

          // There is no limit on the other schema, so we can't have one here
          case (_, None) => (None, false)
        }
      }
    }
  }

  /** Contracts an integer used for a maxium value to meet a given limit.
    *
    * @param current
    *   the current maximum value
    * @param limit
    *   the limit from the other schema
    * @param exclusive
    *   whether the current limit is exclusive or not
    * @param round
    *   the current round of expansion
    *
    * @return
    *   the new maximum value and whether it is exclusive
    */
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def contractInt(
      current: Int,
      limit: Int,
      exclusive: Boolean,
      force: Boolean,
      round: Int
  ): (Option[Int], Boolean) = {
    // If we got here, we must not have exceeded the max number of rounds
    assert(round <= MaxExpandRounds)

    if (current < 0) {
      // TODO We can probably make better choices for negative values
      maybeContractInt(Some(0), Some(limit), exclusive, force, round + 1)
    } else if (exclusive && (current < limit || force)) {
      maybeContractInt(Some(current), Some(limit), false, force, round + 1)
    } else if (
      !force && ((exclusive && current < limit) || (!exclusive && current <= limit))
    ) {
      (Some(current), exclusive)
    } else if (current < 10) {
      maybeContractInt(
        Some(current - 1),
        Some(limit),
        exclusive,
        force,
        round + 1
      )
    } else {
      // Since 2 is a common power, consider the next smallest power
      // but first consider just one less since this occurs frequently
      // (e.g. 255, 65535)
      val log2 = Math.log(current) / Math.log(2)
      val closestPow2 =
        Math.pow(2, (log2).ceil - 1).toInt
      val next2 = if (log2.isWhole) {
        current - 1
      } else {
        closestPow2
      }

      // Otherwise, decrease by the closest power of 10 (examples below)
      // 13 => 12
      // 30 => 20
      // 100 => 99
      // 200 => 190
      // 307 => 300
      val pow10 = Math.pow(10, Math.log10(current).floor - 1).toInt
      val next10 =
        if (current % pow10 == 0)
          current - pow10
        else
          current - (current % pow10)

      if (next2 > next10)
        maybeContractInt(Some(next2), Some(limit), exclusive, force, round + 1)
      else
        maybeContractInt(Some(next10), Some(limit), exclusive, force, round + 1)
    }
  }

  /** Produce a list of all prime factors of an integer. */
  // Source: https://stackoverflow.com/a/30281343/123695
  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def factorize(x: Int): List[Int] = {
    def foo(x: Int, a: Int): List[Int] = if (a * a > x) List(x)
    else
      x % a match {
        case 0 => a :: foo(x / a, a)
        case _ => foo(x, a + 1)
      }
    foo(x, 2).sorted
  }

  /** Determine if one minimum values the values of another, considering whether
    * each minimum is exclusive or not.
    *
    * @param value1
    *   the first minimum value
    * @param exclusive1
    *   whether the first minimum value is exclusive
    * @param value2
    *   the second minimum value
    * @param exclusive2
    *   whether the second minimum value is exclusive
    *
    * @return
    *   true if the second value covers the first, false otherwise
    */
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def isMinCoveredBy[A: Order](
      value1: Option[A],
      exclusive1: Boolean,
      value2: Option[A],
      exclusive2: Boolean
  ): Boolean = {
    if (value2.isEmpty)
      // If the other has no minimum, then it covers the first
      true
    else if (value1.isEmpty)
      // If the other has a minimum and we don't, not covered
      false
    else if (!exclusive1 && exclusive2)
      // If the other is exclusive and we are not,
      // then the other minmum value must be smaller
      value2.get < value1.get
    else
      // Otherwise, minimum value can be less than or equal
      value2.get <= value1.get
  }

  /** Determine if one maximum value covers the values of another, considering
    * whether each maximum is exclusive or not.
    *
    * @param value1
    *   the first maximum value
    * @param exclusive1
    *   whether the first maximum value is exclusive
    * @param value2
    *   the second maximum value
    * @param exclusive2
    *   whether the second maximum value is exclusive
    * @return
    *   true if the second value covers the first, false otherwise
    */
  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def isMaxCoveredBy[A: Order](
      value1: Option[A],
      exclusive1: Boolean,
      value2: Option[A],
      exclusive2: Boolean
  ): Boolean = {
    if (value2.isEmpty)
      // If the other has no maximum, then it covers the first
      true
    else if (value1.isEmpty)
      // If the other has a maximum and we don't, not covered
      false
    else if (!exclusive1 && exclusive2)
      // If the other is exclusive and we are not,
      // then the other maximum value must be larger
      value2.get > value1.get
    else
      // Otherwise, maximum value can be greater than or equal
      value2.get >= value1.get
  }

  /** Find the maximum of two values or None if neither is specified. */
  def maxOrNone[A: Order](first: Option[A], second: Option[A]): Option[A] =
    (first, second) match {
      case (Some(a), None)    => first
      case (None, Some(b))    => second
      case (Some(a), Some(b)) =>
        if (b > a) second else first
      case (None, None) => None
    }

  /** Find the minimum of two values or None if neither is specified. */
  def minOrNone[A: Order](first: Option[A], second: Option[A]): Option[A] =
    (first, second) match {
      case (Some(a), None)    => first
      case (None, Some(b))    => second
      case (Some(a), Some(b)) =>
        if (b < a) second else first
      case (None, None) => None
    }

  /** Find the intersection of two optional sets or None if neither set is
    * specified.
    */
  def intersectOrNone[A](
      first: Option[Set[A]],
      second: Option[Set[A]]
  ): Option[Set[A]] = (first, second) match {
    case (Some(a), None)    => first
    case (None, Some(b))    => second
    case (Some(a), Some(b)) => Some(a.intersect(b))
    case (None, None)       => None
  }

  /** Find the union of two optional sets or None if neither set is specified.
    */
  def unionOrNone[A](
      first: Option[Set[A]],
      second: Option[Set[A]]
  ): Option[Set[A]] = (first, second) match {
    case (Some(a), None)    => first
    case (None, Some(b))    => second
    case (Some(a), Some(b)) => Some(a.union(b))
    case (None, None)       => None
  }

  /** Find a possible common prefix of two strings. */
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def findCommonPrefix(str1: String, str2: String): String = {
    val prefix = str1.toList
      .zip(str2.toList)
      .takeWhile((s: Tuple2[Char, Char]) => s._1 == s._2)
      .map(_._1)
      .mkString

    // Each string must start with the prefix
    assert(str1.startsWith(prefix) && str2.startsWith(prefix))

    prefix
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

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit final class AnyOps[A](self: A) {
    def ===(other: A): Boolean = self == other

    def =/=(other: A): Boolean = self != other
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit final class BooleanOps(self: Boolean) {
    def ===(other: Boolean): Boolean = self == other

    def =/=(other: Boolean): Boolean = self != other
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit final class StringOps(self: String) {
    def ===(other: String): Boolean = self == other

    def =/=(other: String): Boolean = self != other
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit final class AnyOptionOps[A](self: Option[A]) {
    def ===(other: Option[A]): Boolean = self == other

    def =/=(other: Option[A]): Boolean = self != other
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit final class BigIntOps(self: BigInt) {
    def ===(other: BigInt): Boolean = self == other

    def =/=(other: BigInt): Boolean = self != other
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit final class BigDecimalOps(self: BigDecimal) {
    def ===(other: BigDecimal): Boolean = self == other

    def isInDoubleRange: Boolean = {
      self >= Double.MinValue && self <= Double.MaxValue
    }
  }

  /** Find the greatest common divisor of two integers. */
  @tailrec
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a else gcd(b, a % b)

  /** Find the greatest common divisor of two decimals. */
  @tailrec
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def gcd(a: BigDecimal, b: BigDecimal): BigDecimal =
    if (b == 0) a else gcd(b, a % b)

  /** Find the lowest common mu,tiple of two integers. */
  def lcm(a: BigInt, b: BigInt): BigInt = a * b / gcd(a, b)

  /** Find the lowest common mu,tiple of two decimals. */
  def lcm(a: BigDecimal, b: BigDecimal): BigDecimal = a * b / gcd(a, b)
}

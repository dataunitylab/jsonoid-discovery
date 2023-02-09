package edu.rit.cs.mmior.jsonoid.discovery

import scala.annotation.tailrec

import scalaz._
import Scalaz._

object Helpers {
  val MaxExpandRounds: Int = 10

  def maybeExpandInt(
      current: Option[Int],
      limit: Option[Int],
      exclusive: Boolean,
      round: Int = 1
  ): (Option[Int], Boolean) = {
    if (round > MaxExpandRounds) {
      (None, false)
    } else {
      (current, limit) match {
        case (Some(currentInt), Some(otherInt)) =>
          expandInt(currentInt, otherInt, exclusive, round)

        // We have no limit, keep it that way
        case (None, _) => (None, false)

        // There is no limit on the other schema, so we can't have one here
        case (_, None) => (None, false)
      }
    }
  }

  def expandInt(
      current: Int,
      limit: Int,
      exclusive: Boolean,
      round: Int
  ): (Option[Int], Boolean) = {
    if (current < 0) {
      // TODO We can probably make better choices for negative values
      maybeExpandInt(Some(0), Some(limit), exclusive, round + 1)
    } else if (exclusive && current < limit) {
      maybeExpandInt(Some(current), Some(limit), false, round + 1)
    } else if (
      (exclusive && current > limit) || (!exclusive && current >= limit)
    ) {
      (Some(current), exclusive)
    } else if (current < 10) {
      maybeExpandInt(Some(current + 1), Some(limit), exclusive, round + 1)
    } else {
      // Since 2 is a common power, consider the next closest power
      // but first consider one less since this occurs frequently
      // (e.g. 255, 65535)
      val closestPow2 =
        Math.pow(2, (Math.log(current) / Math.log(2)).ceil).toInt
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
      val pow10 = Math.pow(10, Math.log10(current).floor - 1).toInt
      val next10 = current + pow10 - (current % pow10)
      if (next2 < next10 && next2 > current) {
        maybeExpandInt(Some(next2), Some(limit), exclusive, round + 1)
      } else {
        maybeExpandInt(Some(next10), Some(limit), exclusive, round + 1)
      }
    }
  }

  def maybeContractInt(
      current: Option[Int],
      limit: Option[Int],
      exclusive: Boolean,
      round: Int = 1
  ): (Option[Int], Boolean) = {
    if (round > MaxExpandRounds) {
      (None, false)
    } else {
      (current, limit) match {
        case (Some(currentInt), Some(otherInt)) =>
          contractInt(currentInt, otherInt, exclusive, round)

        // We have no limit, keep it that way
        case (None, _) => (None, false)

        // There is no limit on the other schema, so we can't have one here
        case (_, None) => (None, false)
      }
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def contractInt(
      current: Int,
      limit: Int,
      exclusive: Boolean,
      round: Int
  ): (Option[Int], Boolean) = {
    if (current < 0) {
      // TODO We can probably make better choices for negative values
      maybeContractInt(Some(0), Some(limit), exclusive, round + 1)
    } else if (exclusive && current < limit) {
      maybeContractInt(Some(current), Some(limit), false, round + 1)
    } else if (
      (exclusive && current < limit) || (!exclusive && current <= limit)
    ) {
      (Some(current), exclusive)
    } else if (current < 10) {
      maybeContractInt(Some(current - 1), Some(limit), exclusive, round + 1)
    } else {
      // Since 2 is a common power, consider the next smallest power
      // but first consider just one less since this occurs frequently
      // (e.g. 255, 65535)
      val log2 = Math.log(current) / Math.log(2)
      val closestPow2 =
        Math.pow(2, (log2).ceil - 1).toInt
      val next2 = if (log2.isValidInt) {
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
      val next10 = if (current % pow10 == 0) {
        current - pow10
      } else {
        current - (current % pow10)
      }
      if (next2 > next10) {
        maybeContractInt(Some(next2), Some(limit), exclusive, round + 1)
      } else {
        maybeContractInt(Some(next10), Some(limit), exclusive, round + 1)
      }
    }
  }

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

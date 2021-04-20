package edu.rit.cs.mmior.jsonoid.discovery

import scala.annotation.tailrec
import scala.math.BigDecimal.RoundingMode

import scalaz._
import Scalaz._

object Helpers {
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

  // scalastyle:off method.name
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit final class AnyOps[A](self: A) {
    def ===(other: A): Boolean = self == other
  }
  // scalastyle:on method.name

  @tailrec
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a else gcd(b, a % b)

  @tailrec
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def gcd(a: BigDecimal, b: BigDecimal): BigDecimal = {
    if (a < b) {
      gcd(b, a)
    } else if (b < 0.0001) {
      a
    } else {
      gcd(b, a - (a / b).setScale(0, RoundingMode.FLOOR) * b)
    }
  }
}

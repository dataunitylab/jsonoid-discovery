package edu.rit.cs.mmior.jsonoid.discovery

import scala.annotation.tailrec

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
  // scalastyle:on method.name

  @tailrec
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a else gcd(b, a % b)
}

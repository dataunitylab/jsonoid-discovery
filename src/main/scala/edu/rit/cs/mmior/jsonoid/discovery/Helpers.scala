package edu.rit.cs.mmior.jsonoid.discovery

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
  implicit final class AnyOps[A](self: A) {
    def ===(other: A): Boolean = self == other
  }
}

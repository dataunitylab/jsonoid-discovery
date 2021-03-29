package edu.rit.cs.mmior.jsonoid.discovery


import scalaz._
import Scalaz._


object Helpers {
  def maxOrNone[A: Order](first: Option[A], second: Option[A]): Option[A] =
    for { a <- first; b <- second } yield a max b

  def minOrNone[A: Order](first: Option[A], second: Option[A]): Option[A] =
    for { a <- first; b <- second } yield a min b
}

package edu.rit.cs.mmior.jsonoid.discovery.schemas

import scalaz._
import org.json4s._


trait SchemaProperty[T] {
  // XXX: This should really take and return the same
  //      concrete type, but it does not currently
  def merge(prop: SchemaProperty[T]): SchemaProperty[T]

  def merge(value: T): SchemaProperty[T]

  def toJson: JObject
}

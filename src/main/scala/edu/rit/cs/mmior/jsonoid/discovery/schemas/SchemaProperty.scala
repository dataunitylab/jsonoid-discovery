package edu.rit.cs.mmior.jsonoid.discovery.schemas

import org.json4s._

trait SchemaProperty[T, S <: SchemaProperty[T, _]] {
  // XXX: This should really take and return the same
  //      concrete type, but it does not currently
  def merge(prop: S): S

  def mergeOnlySameType(prop: SchemaProperty[T, _]): S = {
    // XXX This only works if the S is the same as the wildcard type
    //     but this is needed since we can't cast to an unknown type
    merge(prop.asInstanceOf[S])
  }

  def mergeValue(value: T): S

  def toJson: JObject
}

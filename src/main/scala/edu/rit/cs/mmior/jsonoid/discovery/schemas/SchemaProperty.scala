package edu.rit.cs.mmior.jsonoid.discovery.schemas

import org.json4s._


trait SchemaProperty[T] {
  def merge(value: T): SchemaProperty[T]
  def toJson: JObject
}

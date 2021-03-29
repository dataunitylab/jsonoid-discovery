package edu.rit.cs.mmior.jsonoid.discovery

import schemas._


object DiscoverSchema {
  // def discover(jsons: Seq[JValue]): JsonSchema = {
  def discover(): JsonSchema[_] = {
    StringSchema()
  }
}

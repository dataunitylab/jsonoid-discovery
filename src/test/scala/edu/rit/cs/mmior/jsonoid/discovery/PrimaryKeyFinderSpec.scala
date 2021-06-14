package edu.rit.cs.mmior.jsonoid.discovery

import org.json4s.JsonDSL._
import org.json4s._

class PrimaryKeyFinderSpec extends UnitSpec {
  behavior of "PrimaryKeyFinder"

  it should "find possible primary keys" in {
    val jsons: Seq[JValue] = Seq(("a" -> 3) ~ ("b" -> 4), ("a" -> 3) ~ ("b" -> 3))
    val schema = DiscoverSchema.discover(jsons.iterator)
    val primaryKeys = PrimaryKeyFinder.findPrimaryKeys(schema)

    primaryKeys shouldEqual List(PrimaryKey("$.b"))
  }
}

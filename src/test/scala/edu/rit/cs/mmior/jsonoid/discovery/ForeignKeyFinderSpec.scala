package edu.rit.cs.mmior.jsonoid.discovery

import org.json4s.JsonDSL._
import org.json4s._

class ForeignKeyFinderSpec extends UnitSpec {
  behavior of "ForeignKeyFinder"

  it should "find possible foreign keys" in {
    val jsons: Seq[JValue] = Seq(("a" -> 3) ~ ("b" -> 4), ("a" -> 3) ~ ("b" -> 3))
    val schema = DiscoverSchema.discover(jsons.iterator)
    val foreignKeys = ForeignKeyFinder.findForeignKeys(schema)

    foreignKeys shouldEqual List(ForeignKey("$.a", "$.b"))
  }
}

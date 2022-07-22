package edu.rit.cs.mmior.jsonoid.discovery

import org.json4s.JsonDSL._
import org.json4s._

class ForeignKeyFinderSpec extends UnitSpec {
  behavior of "ForeignKeyFinder"

  implicit val er: EquivalenceRelation =
    EquivalenceRelations.KindEquivalenceRelation

  it should "find possible foreign keys that are integers" in {
    val jsons: Seq[JValue] =
      Seq(("a" -> 3) ~ ("b" -> 4), ("a" -> 3) ~ ("b" -> 3))
    val schema = DiscoverSchema.discover(jsons.iterator)
    val foreignKeys = ForeignKeyFinder.findForeignKeys(schema)

    foreignKeys shouldEqual List(ForeignKey("$.a", "$.b"))
  }

  it should "find possible foreign keys that are strings" in {
    val jsons: Seq[JValue] =
      Seq(("a" -> "c") ~ ("b" -> "d"), ("a" -> "c") ~ ("b" -> "c"))
    val schema = DiscoverSchema.discover(jsons.iterator)
    val foreignKeys = ForeignKeyFinder.findForeignKeys(schema)

    foreignKeys shouldEqual List(ForeignKey("$.a", "$.b"))
  }

  it should "find possible foreign keys that are numbers" in {
    val jsons: Seq[JValue] =
      Seq(("a" -> 3.2) ~ ("b" -> 6.5), ("a" -> 3.2) ~ ("b" -> 3.2))
    val schema = DiscoverSchema.discover(jsons.iterator)
    val foreignKeys = ForeignKeyFinder.findForeignKeys(schema)

    foreignKeys shouldEqual List(ForeignKey("$.a", "$.b"))
  }
}

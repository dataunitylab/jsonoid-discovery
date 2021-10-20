package edu.rit.cs.mmior.jsonoid.discovery

import schemas.NullSchema

import org.json4s.JsonDSL._
import org.json4s._

class PrimaryKeyFinderSpec extends UnitSpec {
  behavior of "PrimaryKeyFinder"

  implicit val er: EquivalenceRelation = EquivalenceRelations.KindEquivalenceRelation

  it should "find possible primary keys which are integers" in {
    val jsons: Seq[JValue] = Seq(("a" -> 3) ~ ("b" -> 4), ("a" -> 3) ~ ("b" -> 3))
    val schema = DiscoverSchema.discover(jsons.iterator)
    val primaryKeys = PrimaryKeyFinder.findPrimaryKeys(schema)

    primaryKeys shouldEqual List(PrimaryKey("$.b"))
  }

  it should "find possible primary keys which are strings" in {
    val jsons: Seq[JValue] = Seq(("a" -> "c") ~ ("b" -> "d"), ("a" -> "c") ~ ("b" -> "c"))
    val schema = DiscoverSchema.discover(jsons.iterator)
    val primaryKeys = PrimaryKeyFinder.findPrimaryKeys(schema)

    primaryKeys shouldEqual List(PrimaryKey("$.b"))
  }

  it should "find possible primary keys which are numbers" in {
    val jsons: Seq[JValue] = Seq(("a" -> 3.2) ~ ("b" -> 6.5), ("a" -> 3.2) ~ ("b" -> 3.2))
    val schema = DiscoverSchema.discover(jsons.iterator)
    val primaryKeys = PrimaryKeyFinder.findPrimaryKeys(schema)

    primaryKeys shouldEqual List(PrimaryKey("$.b"))
  }

  it should "not find primary keys if they do not exist" in {
    val jsons: Seq[JValue] = Seq(("a" -> 3) ~ ("b" -> 3), ("a" -> 3) ~ ("b" -> 3))
    val schema = DiscoverSchema.discover(jsons.iterator)
    val primaryKeys = PrimaryKeyFinder.findPrimaryKeys(schema)

    primaryKeys shouldEqual List()
  }

  it should "not find primary keys on schemas which are not ObjectSchema" in {
    val primaryKeys = PrimaryKeyFinder.findPrimaryKeys(NullSchema())

    primaryKeys shouldEqual List()
  }
}

package io.github.dataunitylab.jsonoid.discovery

import schemas.NullSchema

import org.json4s.JsonDSL._
import org.json4s._

class PrimaryKeyFinderSpec extends UnitSpec {
  behavior of "PrimaryKeyFinder"

  it should "find possible primary keys which are integers" in {
    val jsons: Seq[JValue] =
      Seq(("a" -> 3) ~ ("b" -> 4), ("a" -> 3) ~ ("b" -> 3))
    val schema = DiscoverSchema.discover(jsons.iterator)
    val primaryKeys = PrimaryKeyFinder.findPrimaryKeys(schema)

    primaryKeys shouldEqual List(PrimaryKey("$.b"))
  }

  it should "find possible primary keys which are strings" in {
    val jsons: Seq[JValue] =
      Seq(("a" -> "c") ~ ("b" -> "d"), ("a" -> "c") ~ ("b" -> "c"))
    val schema = DiscoverSchema.discover(jsons.iterator)
    val primaryKeys = PrimaryKeyFinder.findPrimaryKeys(schema)

    primaryKeys shouldEqual List(PrimaryKey("$.b"))
  }

  it should "find possible primary keys which are numbers" in {
    val jsons: Seq[JValue] =
      Seq(("a" -> 3.2) ~ ("b" -> 6.5), ("a" -> 3.2) ~ ("b" -> 3.2))
    val schema = DiscoverSchema.discover(jsons.iterator)
    val primaryKeys = PrimaryKeyFinder.findPrimaryKeys(schema)

    primaryKeys shouldEqual List(PrimaryKey("$.b"))
  }

  it should "not find primary keys if they do not exist" in {
    val jsons: Seq[JValue] =
      Seq(("a" -> 3) ~ ("b" -> 3), ("a" -> 3) ~ ("b" -> 3))
    val schema = DiscoverSchema.discover(jsons.iterator)
    val primaryKeys = PrimaryKeyFinder.findPrimaryKeys(schema)

    primaryKeys shouldEqual List()
  }

  it should "prefer shorter primary keys" in {
    val jsons: Seq[JValue] =
      Seq(
        ("a" -> "thisvalueisunique") ~ ("b" -> "1"),
        ("a" -> "thisvalueisalsounique") ~ ("b" -> "2")
      )
    val schema = DiscoverSchema.discover(jsons.iterator)
    val primaryKeys = PrimaryKeyFinder.findPrimaryKeys(schema)

    primaryKeys shouldEqual List(PrimaryKey("$.b"))
  }

  it should "prefer primary keys with a given prefix" in {
    val jsons: Seq[JValue] =
      Seq(("ida" -> 1) ~ ("b" -> 1), ("ida" -> 2) ~ ("b" -> 2))
    val schema = DiscoverSchema.discover(jsons.iterator)
    val primaryKeys = PrimaryKeyFinder.findPrimaryKeys(schema)

    primaryKeys shouldEqual List(PrimaryKey("$.ida"))
  }

  it should "prefer primary keys which are integers or strings" in {
    val jsons: Seq[JValue] =
      Seq(
        ("a" -> 1) ~ ("b" -> "1") ~ ("c" -> 1.0),
        ("a" -> 2) ~ ("b" -> "2") ~ ("c" -> 2.0)
      )
    val schema = DiscoverSchema.discover(jsons.iterator)
    val primaryKeys = PrimaryKeyFinder.findPrimaryKeys(schema)

    primaryKeys.toSet shouldEqual Set(PrimaryKey("$.a"), PrimaryKey("$.b"))
  }

  it should "prefer keys with lower depth" in {
    val jsons: Seq[JValue] =
      Seq(("a" -> 1) ~ ("b" -> ("c" -> 1)), ("a" -> 2) ~ ("b" -> ("c" -> 2)))
    val schema = DiscoverSchema.discover(jsons.iterator)
    val primaryKeys = PrimaryKeyFinder.findPrimaryKeys(schema)

    primaryKeys shouldEqual List(PrimaryKey("$.a"))
  }

  it should "not find primary keys on schemas which are not ObjectSchema" in {
    val primaryKeys = PrimaryKeyFinder.findPrimaryKeys(NullSchema())

    primaryKeys shouldEqual List()
  }
}

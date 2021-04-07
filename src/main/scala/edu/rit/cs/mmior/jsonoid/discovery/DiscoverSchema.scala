package edu.rit.cs.mmior.jsonoid.discovery

import java.io.File
import scala.io.Source
import scala.annotation.tailrec

import org.json4s._
import org.json4s.jackson.JsonMethods._

import schemas._

object DiscoverSchema {
  def discover(jsons: Seq[JValue]): JsonSchema[_] = {
    jsons.map(discoverFromValue(_)).fold(ZeroSchema())(_.merge(_))
  }

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def discoverFromValue(value: JValue): JsonSchema[_] = {
    value match {
      case JArray(items) => ArraySchema(items.map(discoverFromValue))
      case JBool(bool)   => BooleanSchema(bool)
      case JDecimal(dec) => NumberSchema(dec)
      case JDouble(dbl)  => NumberSchema(dbl)
      case JInt(int)     => IntegerSchema(int)
      case JLong(long)   => IntegerSchema(long)
      case JNothing      => NullSchema()
      case JNull         => NullSchema()
      case JObject(fields) =>
        ObjectSchema(
          fields
            .map { case (k, v) =>
              (k, discoverFromValue(v))
            }
            .asInstanceOf[Seq[(String, JsonSchema[_])]]
            .toMap
        )
      case JSet(items)  => ArraySchema(items.map(discoverFromValue).toList)
      case JString(str) => StringSchema(str)
    }
  }

  def jsonFromSource(source: Source): Seq[JValue] = {
    source.getLines().map(parse(_)).toSeq
  }
}

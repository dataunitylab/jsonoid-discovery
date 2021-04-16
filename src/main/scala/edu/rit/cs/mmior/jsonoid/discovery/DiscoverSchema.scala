package edu.rit.cs.mmior.jsonoid.discovery

import java.io.File
import scala.io.Source

import scopt.OptionParser
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._

import schemas._

final case class Config(
    input: Option[File] = None
)

object DiscoverSchema {
  def discover(jsons: Seq[JValue]): JsonSchema[_] = {
    jsons.map(discoverFromValue(_)).fold(ZeroSchema())(_.merge(_))
  }

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def discoverFromValue(value: JValue): JsonSchema[_] = {
    value match {
      case JArray(items)   => ArraySchema(items.map(discoverFromValue))
      case JBool(bool)     => BooleanSchema(bool)
      case JDecimal(dec)   => NumberSchema(dec)
      case JDouble(dbl)    => NumberSchema(dbl)
      case JInt(int)       => IntegerSchema(int)
      case JLong(long)     => IntegerSchema(long)
      case JNothing        => NullSchema()
      case JNull           => NullSchema()
      case JObject(fields) => discoverObjectFields(fields)
      case JSet(items)     => ArraySchema(items.map(discoverFromValue).toList)
      case JString(str)    => StringSchema(str)
    }
  }

  def discoverObjectFields(fields: Seq[JField]): JsonSchema[_] = {
    ObjectSchema(
      fields
        .map { case (k, v) =>
          (k, discoverFromValue(v))
        }
        .asInstanceOf[Seq[(String, JsonSchema[_])]]
        .toMap
    )
  }

  def jsonFromSource(source: Source): Seq[JValue] = {
    source.getLines().map(parse(_)).toSeq
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def main(args: Array[String]): Unit = {
    val parser = new OptionParser[Config]("jsonoid-discover") {
      head("jsonoid-discover", "0.1.0-SNAPSHOT")

      help("help")
      version("version")

      arg[File]("<input>")
        .optional()
        .action((x, c) => c.copy(input = Some(x)))
        .text("a JSON file to perform discovery on, one object per line")
    }

    parser.parse(args, Config()) match {
      case Some(config) =>
        val source = config.input match {
          case Some(file) => Source.fromFile(file)
          case None       => Source.stdin
        }

        val jsons = jsonFromSource(source)
        val schema = discover(jsons)
        val schemaObj: JObject =
          ("$schema" -> "https://json-schema.org/draft/2019-09/schema")
        println(compact(render(schema.toJson.merge(schemaObj))))
      case None =>
    }
  }
}

package edu.rit.cs.mmior.jsonoid.discovery

import java.io.File
import java.io.FileOutputStream
import scala.io.Source

import scopt.OptionParser
import org.json4s._
import org.json4s.jackson.JsonMethods._

import edu.rit.cs.mmior.jsonoid.BuildInfo
import schemas._

final case class Config(
    input: Option[File] = None,
    writeValues: Option[File] = None,
    propertySet: PropertySet = PropertySets.AllProperties
)

object DiscoverSchema {
  def discover(
      jsons: Iterator[JValue],
      propSet: PropertySet = PropertySets.AllProperties
  ): JsonSchema[_] = {
    jsons.map(discoverFromValue(_, propSet)).fold(ZeroSchema())(_.merge(_))
  }

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def discoverFromValue(
      value: JValue,
      propSet: PropertySet = PropertySets.AllProperties
  ): JsonSchema[_] = {
    value match {
      case JArray(items) =>
        ArraySchema(items.map(discoverFromValue(_, propSet)))(propSet)
      case JBool(bool)     => BooleanSchema(bool)
      case JDecimal(dec)   => NumberSchema(dec)(propSet)
      case JDouble(dbl)    => NumberSchema(dbl)(propSet)
      case JInt(int)       => IntegerSchema(int)(propSet)
      case JLong(long)     => IntegerSchema(long)(propSet)
      case JNothing        => NullSchema()
      case JNull           => NullSchema()
      case JObject(fields) => discoverObjectFields(fields, propSet)
      case JSet(items) =>
        ArraySchema(items.map(discoverFromValue(_, propSet)).toList)(propSet)
      case JString(str) => StringSchema(str)(propSet)
    }
  }

  def discoverObjectFields(
      fields: Seq[JField],
      propSet: PropertySet
  ): JsonSchema[_] = {
    ObjectSchema(
      fields
        .map { case (k, v) =>
          (k, discoverFromValue(v, propSet))
        }
        .asInstanceOf[Seq[(String, JsonSchema[_])]]
        .toMap
    )(propSet)
  }

  def jsonFromSource(source: Source): Iterator[JValue] = {
    source.getLines().map(parse(_))
  }

  def transformSchema(schema: JsonSchema[_]): JsonSchema[_] = {
    EnumTransformer.transformSchema(schema)
  }

  implicit val propertySetRead: scopt.Read[PropertySet] =
    scopt.Read.reads(typeName =>
      typeName match {
        case "All"    => PropertySets.AllProperties
        case "Min"    => PropertySets.MinProperties
        case "Simple" => PropertySets.SimpleProperties
      }
    )

  // $COVERAGE-OFF$ No automated testing of CLI
  @SuppressWarnings(
    Array(
      "org.wartremover.warts.NonUnitStatements",
      "org.wartremover.warts.OptionPartial"
    )
  )
  def main(args: Array[String]): Unit = {
    val parser = new OptionParser[Config]("jsonoid-discover") {
      head("jsonoid-discover", BuildInfo.version)

      help("help")

      arg[File]("<input>")
        .optional()
        .action((x, c) => c.copy(input = Some(x)))
        .text("a JSON file to perform discovery on, one object per line")

      opt[File]('v', "values")
        .action((x, c) => c.copy(writeValues = Some(x)))
        .valueName("<file>")
        .text("a file where a table of collected values should be written")

      opt[PropertySet]('p', "prop")
        .action((x, c) => c.copy(propertySet = x))
        .text("the set of properties to calculate [All, Min, Simple]")
    }

    parser.parse(args, Config()) match {
      case Some(config) =>
        val source = config.input match {
          case Some(file) => Source.fromFile(file)
          case None       => Source.stdin
        }

        val jsons = jsonFromSource(source)
        val schema = discover(jsons, config.propertySet)

        if (!config.writeValues.isEmpty) {
          val objectSchema = schema.asInstanceOf[ObjectSchema]
          val outputStream = new FileOutputStream(config.writeValues.get)
          ValueTableGenerator.writeValueTable(objectSchema, outputStream)
        }

        val transformedSchema: ObjectSchema =
          transformSchema(schema).asInstanceOf[ObjectSchema]
        println(compact(render(transformedSchema.toJsonSchema)))
      case None =>
    }
  }
  // $COVERAGE-ON$
}

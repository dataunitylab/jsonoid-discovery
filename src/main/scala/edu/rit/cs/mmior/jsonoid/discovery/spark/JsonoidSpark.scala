package edu.rit.cs.mmior.jsonoid.discovery
package spark

import schemas._

import scopt.OptionParser
import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import org.json4s.jackson.JsonMethods._

final case class Config(
    input: String = "",
    propertySet: PropertySet = PropertySets.AllProperties
)

object JsonoidSpark {
  implicit val propertySetRead: scopt.Read[PropertySet] =
    scopt.Read.reads(typeName => typeName match {
      case "All" => PropertySets.AllProperties
      case "Min" => PropertySets.MinProperties
    })

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def main(args: Array[String]): Unit = {
    val parser = new OptionParser[Config]("jsonoid-discover") {
      head("jsonoid-discover")

      help("help")

      arg[String]("<input>")
        .action((x, c) => c.copy(input = x))
        .text("a JSON file to perform discovery on, one object per line")

      opt[PropertySet]('p', "prop")
        .action((x, c) => c.copy(propertySet = x))
        .text("the set of properties to calculate [All, Min]")
    }

    parser.parse(args, Config()) match {
      case Some(config) =>
        val conf = new SparkConf().setAppName("JSONoid")
        val sc = new SparkContext(conf)
        val jsonRdd = JsonoidRDD.fromStringRDD(
          sc.textFile(config.input),
          config.propertySet
        )
        val schema = jsonRdd.reduceSchemas
        val transformedSchema: ObjectSchema =
          DiscoverSchema.transformSchema(schema).asInstanceOf[ObjectSchema]
        println(compact(render(transformedSchema.toJsonSchema)))
      case None =>
    }
  }
}

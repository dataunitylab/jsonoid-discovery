package io.github.dataunitylab.jsonoid.discovery
package spark

import schemas._
import Helpers._

import scopt.OptionParser
import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import org.json4s.jackson.JsonMethods._

/** Internal configuration for JSONoid on Spark. */
private final case class Config(
    input: String = "",
    propertySet: PropertySet = PropertySets.AllProperties,
    addDefinitions: Boolean = false,
    detectDynamic: Boolean = false,
    detectDisjoint: Boolean = false,
    treeReduce: Boolean = false,
)

object JsonoidSpark {
  // $COVERAGE-OFF$ No automated testing of CLI
  implicit val propertySetRead: scopt.Read[PropertySet] =
    scopt.Read.reads(typeName =>
      typeName match {
        case "All"    => PropertySets.AllProperties
        case "Min"    => PropertySets.MinProperties
        case "Simple" => PropertySets.SimpleProperties
      }
    )

  @SuppressWarnings(
    Array(
      "org.wartremover.warts.NonUnitStatements",
      "org.wartremover.warts.Var"
    )
  )
  def main(args: Array[String]): Unit = {
    val parser = new OptionParser[Config]("jsonoid-discover") {
      head("jsonoid-discover", BuildInfo.version)

      help("help")

      arg[String]("<input>")
        .action((x, c) => c.copy(input = x))
        .text("a JSON file to perform discovery on, one object per line")

      opt[PropertySet]('p', "prop")
        .action((x, c) => c.copy(propertySet = x))
        .text("the set of properties to calculate [All, Min, Simple]")

      opt[Unit]('d', "add-definitions")
        .action((x, c) => c.copy(addDefinitions = true))
        .text("extract similar objects to create definitions")

      opt[Unit]('y', "detect-dynamic")
        .action((x, c) => c.copy(detectDynamic = true))
        .text("detect objects with dynamic keys")

      opt[Unit]('j', "detect-disjoint")
        .action((x, c) => c.copy(detectDisjoint = true))
        .text("detect objects with disjoint keys")

      opt[Unit]('t', "tree-reduce")
        .action((x, c) => c.copy(treeReduce = true))
        .text("use treeReduce for schema reduction")
    }

    parser.parse(args, Config()) match {
      case Some(config) =>
        val conf = new SparkConf().setAppName("JSONoid")
        val sc = new SparkContext(conf)
        val er = EquivalenceRelations.KindEquivalenceRelation
        val p = JsonoidParams().withER(er).withPropertySet(config.propertySet)
        val jsonRdd = JsonoidRDD.fromStringRDD(
          sc.textFile(config.input)
        )(p)
        var schema: ObjectSchema = if (config.treeReduce) {
          jsonRdd.treeReduceSchemas().asInstanceOf[ObjectSchema]
        } else {
          jsonRdd.reduceSchemas().asInstanceOf[ObjectSchema]
        }

        // Skip transformation if we know the required properties don't exist
        if (!(config.propertySet === PropertySets.MinProperties)) {
          schema = DiscoverSchema
            .transformSchema(
              schema,
              None,
              config.addDefinitions,
              config.detectDynamic,
              config.detectDisjoint
            )(p)
            .asInstanceOf[ObjectSchema]
        }

        println(compact(render(schema.toJsonSchema()(p))))
      case None =>
    }
  }
  // $COVERAGE-ON$
}

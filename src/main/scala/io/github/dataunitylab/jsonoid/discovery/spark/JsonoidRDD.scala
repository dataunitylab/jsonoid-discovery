package io.github.dataunitylab.jsonoid.discovery
package spark

import scala.language.implicitConversions

import org.apache.spark.rdd.RDD
import org.json4s.jackson.JsonMethods._

import schemas._

object JsonoidRDD {

  /** Create a JSONoid RDD from an RDD of JSON strings.
    *
    * @param rdd
    *   the RDD of JSON strings
    * @param propSet
    *   the set of properties which should be discovered
    */
  def fromStringRDD(
      rdd: RDD[String]
  )(implicit p: JsonoidParams): JsonoidRDD = {
    val discoverFromString = (jsonString: String) =>
      DiscoverSchema.discoverFromValue(parse(jsonString))
    new JsonoidRDD(rdd.flatMap(discoverFromString))(p)
  }

  implicit def unwrapJsonoidRdd(jsonoidRdd: JsonoidRDD): RDD[JsonSchema[_]] =
    jsonoidRdd.rdd
}

/** A JSONoid RDD is an RDD of schema objects.
  */
@SerialVersionUID(2081137182276371163L)
class JsonoidRDD(val rdd: RDD[JsonSchema[_]])(implicit
    p: JsonoidParams
) extends Serializable {

  /** Reduce the RDD down to a single schema. */
  def reduceSchemas(): JsonSchema[_] = {
    rdd.fold(ZeroSchema())(_.merge(_)(p))
  }

  /** Reduce the RDD down to a single schema using tree reduction. */
  def treeReduceSchemas(depth: Int = 2): JsonSchema[_] = {
    rdd.treeReduce(_.merge(_)(p), depth)
  }
}

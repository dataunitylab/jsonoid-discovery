package edu.rit.cs.mmior.jsonoid.discovery
package spark

import scala.language.implicitConversions

import org.apache.spark.rdd.RDD
import org.json4s._
import org.json4s.jackson.JsonMethods._

import schemas._

object JsonoidRDD {
  def fromStringRDD(
      rdd: RDD[String],
      propSet: PropertySet = PropertySets.AllProperties
  )(implicit er: EquivalenceRelation): JsonoidRDD = {
    val discoverFromString = (jsonString: String) =>
      DiscoverSchema.discoverFromValue(parse(jsonString), propSet)
    new JsonoidRDD(rdd.map(discoverFromString))(er)
  }

  implicit def unwrapJsonoidRdd(jsonoidRdd: JsonoidRDD): RDD[JsonSchema[_]] =
    jsonoidRdd.rdd
}

class JsonoidRDD(val rdd: RDD[JsonSchema[_]])(implicit
    er: EquivalenceRelation
) extends Serializable {
  def reduceSchemas(): JsonSchema[_] = {
    rdd.fold(ZeroSchema())(_.merge(_)(er))
  }
}

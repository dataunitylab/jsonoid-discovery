package edu.rit.cs.mmior.jsonoid.discovery
package spark

import schemas._

import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import org.json4s.jackson.JsonMethods._

object JsonoidSpark {
  def main(args: Array[String]): Unit = {
    val conf = new SparkConf().setAppName("JSONoid")
    val sc = new SparkContext(conf)
    val jsonRdd = JsonoidRDD.fromStringRDD(sc.textFile(args(0)))
    val schema = jsonRdd.reduceSchemas
    val transformedSchema: ObjectSchema =
      DiscoverSchema.transformSchema(schema).asInstanceOf[ObjectSchema]
    println(compact(render(transformedSchema.toJsonSchema)))
  }
}

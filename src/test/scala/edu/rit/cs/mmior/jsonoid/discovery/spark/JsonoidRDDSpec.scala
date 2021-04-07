package edu.rit.cs.mmior.jsonoid.discovery
package spark

import org.apache.spark.{SparkConf, SparkContext}

import schemas.ObjectSchema

class JsonoidRDDSpec extends UnitSpec {
  behavior of "JsonoidRDD"

  it should "produce a schema with Spark" in {
    val jsons = Seq("""{"a": "bar", "b": true, "c": null}""", """{"a": "foo", "b": 3.2,"c": 3}""")
    val conf = new SparkConf().setMaster("local")
                              .setAppName("Test")
                              .set("spark.driver.host", "127.0.0.1")
    val sc = new SparkContext(conf)
    val rdd = sc.parallelize(jsons)

    val jsonoidRdd = JsonoidRDD.fromStringRDD(rdd)
    val schema = jsonoidRdd.reduceSchemas()

    schema shouldBe a[ObjectSchema]
  }
}

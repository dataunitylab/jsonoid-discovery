package edu.rit.cs.dataunitylab.jsonoid.discovery
package spark

import org.apache.spark.{SparkConf, SparkContext}

import schemas.{ObjectSchema, PropertySets}

import UnitSpec._

class JsonoidRDDSpec extends UnitSpec {
  behavior of "JsonoidRDD"

  it should "produce a schema with Spark" in {
    val jsons = Seq(
      """{"a": "bar", "b": true, "c": null}""",
      """{"a": "foo", "b": 3.2,"c": 3}"""
    )
    val conf = new SparkConf()
      .setMaster("local")
      .setAppName("Test")
      .set("spark.driver.host", "127.0.0.1")
    val sc = new SparkContext(conf)
    val rdd = sc.parallelize(jsons)

    val jsonoidRdd = JsonoidRDD.fromStringRDD(rdd, PropertySets.MinProperties)
    val schema = jsonoidRdd.reduceSchemas()

    val cp = new Checkpoint()

    cp { schema shouldBe a[ObjectSchema] }
    cp { schema.properties should have size 2 }
    cp { jsonoidRdd.count() should be(2) }

    cp.reportAll()
  }
}

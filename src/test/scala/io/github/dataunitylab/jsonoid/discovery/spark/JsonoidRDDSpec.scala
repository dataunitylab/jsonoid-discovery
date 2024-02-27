package io.github.dataunitylab.jsonoid.discovery
package spark

import org.apache.spark.{SparkConf, SparkContext}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import schemas.{ObjectSchema, PropertySets}

class JsonoidRDDSpec extends UnitSpec with ScalaCheckPropertyChecks {
  behavior of "JsonoidRDD"

  val table = Table(
    ("treeReduce"),
    (true),
    (false)
  )

  it should "produce a schema with Spark" in {
    forAll(table) { (treeReduce) =>
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

      val params = JsonoidParams().withPropertySet(PropertySets.MinProperties)
      val jsonoidRdd = JsonoidRDD.fromStringRDD(rdd)(params)
      val schema = if (treeReduce) {
        jsonoidRdd.treeReduceSchemas()
      } else {
        jsonoidRdd.reduceSchemas()
      }

      val cp = new Checkpoint()

      cp { schema shouldBe a[ObjectSchema] }
      cp { schema.properties should have size 2 }
      cp { jsonoidRdd.count() should be(2) }

      cp.reportAll()

      sc.stop()
    }
  }
}

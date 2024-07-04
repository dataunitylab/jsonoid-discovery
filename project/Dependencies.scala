import sbt._

object Dependencies {
  // Runtime
  // XXX json4s cannot currently be upgrade to maintain compatibility with Spark
  lazy val json4s = "org.json4s" %% "json4s-jackson" % "3.7.0-M11"
  lazy val json4sScalaz = "org.json4s" %% "json4s-scalaz" % "3.7.0-M11"

  lazy val bloomFilter = "io.github.michaelmior" % "bloomfilter" % "0.10.1"
  lazy val dbscan = "io.github.michaelmior" % "dbscan" % "0.2"
  lazy val ddSketch = "com.datadoghq" % "sketches-java" % "0.8.3"
  lazy val fuzzySets = "io.github.dataunitylab" %% "fuzzy-sets" % "0.4.0"
  lazy val hyperLogLog = "com.github.prasanthj" % "hyperloglog" % "1.1"
  lazy val jsonSchemaValidator =
    "com.networknt" % "json-schema-validator" % "1.4.3"
  lazy val openLocationCode =
    "com.google.openlocationcode" % "openlocationcode" % "1.0.4"
  lazy val scopt = "com.github.scopt" %% "scopt" % "4.1.0"
  lazy val scalaCsv = "com.github.tototoshi" %% "scala-csv" % "2.0.0"
  lazy val spark = "org.apache.spark" %% "spark-core" % "3.5.1"
  lazy val sparkSql = "org.apache.spark" %% "spark-sql" % "3.5.1"
  lazy val validator = "commons-validator" % "commons-validator" % "1.9.0"

  // Test
  lazy val jazzer = "com.code-intelligence" % "jazzer" % "0.22.1"
  lazy val scalactic = "org.scalactic" %% "scalactic" % "3.2.19"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.19"
  lazy val scalaTestPlus = "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.18.0"

  // Overrides
  // XXX Bundled version is vulnerable to CVE-2018-10237
  lazy val guava = "com.google.guava" % "guava" % "33.0.0-jre"
  // XXX This is necessary for Spark version consistency
  lazy val jacksonDatabind =
    "com.fasterxml.jackson.core" % "jackson-databind" % "2.15.2"
  // XXX Version bundled with Spark is vulnerable to CVE-2022-3171
  lazy val protobuf = "com.google.protobuf" % "protobuf-java" % "3.25.3"
  // XXX Bundled version is vulnerable to CVE-2023-34455
  lazy val snappyJava = "org.xerial.snappy" % "snappy-java" % "1.1.10.5"
}

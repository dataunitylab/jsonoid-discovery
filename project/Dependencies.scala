import sbt._

object Dependencies {
  // Runtime
  // XXX json4s cannot currently be upgrade to maintain compatibility with Spark
  lazy val json4s              = "org.json4s"                 %% "json4s-jackson"         % "3.7.0-M2"
  lazy val json4sScalaz        = "org.json4s"                 %% "json4s-scalaz"          % "3.7.0-M2"

  lazy val bloomFilter         = "com.sangupta"               % "bloomfilter"             % "0.9.1-SNAPSHOT"
  lazy val hyperLogLog         = "com.github.prasanthj"       %  "hyperloglog"            % "1.1"
  lazy val jsonSchemaValidator = "com.networknt"              %  "json-schema-validator"  % "1.0.49"
  lazy val scopt               = "com.github.scopt"           %% "scopt"                  % "4.0.1"
  lazy val spark               = "org.apache.spark"           %% "spark-core"             % "3.1.1"
  lazy val sparkSql            = "org.apache.spark"           %% "spark-sql"              % "3.1.1"

  // Test
  lazy val scalaTest           = "org.scalatest"              %% "scalatest"              % "3.2.7"

  // Overrides
  // XXX This is necessary for Spark version consistency
  lazy val jacksonDatabind     = "com.fasterxml.jackson.core" %  "jackson-databind"       % "2.10.5.1"
}

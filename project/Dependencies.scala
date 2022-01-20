import sbt._

object Dependencies {
  // Runtime
  // XXX json4s cannot currently be upgrade to maintain compatibility with Spark
  lazy val json4s              = "org.json4s"                 %% "json4s-jackson"         % "3.5.3"
  lazy val json4sScalaz        = "org.json4s"                 %% "json4s-scalaz"          % "3.5.3"

  lazy val bloomFilter         = "com.sangupta"               % "bloomfilter"             % "0.9.1-SNAPSHOT"
  lazy val dbscan              = "com.github.chrfrantz"       % "DBSCAN"                  % "1de90e7"
  lazy val fuzzySets           = "com.github.dataunitylab"    % "fuzzy-sets"              % "v0.2.0"
  lazy val hyperLogLog         = "com.github.prasanthj"       %  "hyperloglog"            % "1.1"
  lazy val jsonSchemaValidator = "com.networknt"              %  "json-schema-validator"  % "1.0.49"
  lazy val scopt               = "com.github.scopt"           %% "scopt"                  % "4.0.1"
  lazy val scalaCsv            = "com.github.tototoshi"       %% "scala-csv"              % "1.3.10"
  lazy val spark               = "org.apache.spark"           %% "spark-core"             % "2.4.8"
  lazy val sparkSql            = "org.apache.spark"           %% "spark-sql"              % "2.4.8"

  // Test
  lazy val scalaTest           = "org.scalatest"              %% "scalatest"              % "3.2.10"

  // Overrides
  // XXX This is necessary for Spark version consistency
  lazy val jacksonDatabind     = "com.fasterxml.jackson.core" %  "jackson-databind"       % "2.6.7.3"
}

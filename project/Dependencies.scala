import sbt._

object Dependencies {
  // Runtime
  // XXX json4s cannot currently be upgrade to maintain compatibility with Spark
  lazy val json4s              = "org.json4s"                 %% "json4s-jackson"        % "3.7.0-M11"
  lazy val json4sScalaz        = "org.json4s"                 %% "json4s-scalaz"         % "3.7.0-M11"

  lazy val bloomFilter         = "com.sangupta"                % "bloomfilter"           % "0.9.1-SNAPSHOT"
  lazy val dbscan              = "com.github.chrfrantz"        % "DBSCAN"                % "1de90e7"
  lazy val ddSketch            = "com.datadoghq"               % "sketches-java"         % "0.8.2"
  lazy val fuzzySets           = "io.github.dataunitylab"     %% "fuzzy-sets"            % "0.4.0"
  lazy val hyperLogLog         = "com.github.prasanthj"        % "hyperloglog"           % "1.1"
  lazy val jsonSchemaValidator = "com.networknt"               % "json-schema-validator" % "1.0.87"
  lazy val openLocationCode    = "com.google.openlocationcode" % "openlocationcode"      % "1.0.4"
  lazy val scopt               = "com.github.scopt"           %% "scopt"                 % "4.1.0"
  lazy val scalaCsv            = "com.github.tototoshi"       %% "scala-csv"             % "1.3.10"
  lazy val spark               = "org.apache.spark"           %% "spark-core"            % "3.4.0"
  lazy val sparkSql            = "org.apache.spark"           %% "spark-sql"             % "3.4.0"
  lazy val validator           = "commons-validator"           % "commons-validator"     % "1.7"

  // Test
  lazy val jazzer              = "com.code-intelligence"       % "jazzer"                 % "0.16.1"
  lazy val scalactic           = "org.scalactic"              %% "scalactic"              % "3.2.17"
  lazy val scalaTest           = "org.scalatest"              %% "scalatest"              % "3.2.17"
  lazy val scalaTestPlus       = "org.scalatestplus"          %% "scalacheck-1-17"        % "3.2.17.0"
  lazy val scalaCheck          = "org.scalacheck"             %% "scalacheck"             % "1.17.0"


  // Overrides
  // XXX Bundled version is vulnerable to CVE-2018-10237
  lazy val guava               = "com.google.guava"             % "guava"                 % "31.1-jre"
  // XXX This is necessary for Spark version consistency
  lazy val jacksonDatabind     = "com.fasterxml.jackson.core" %  "jackson-databind"       % "2.14.3"
  // XXX Version bundled with Spark is vulnerable to CVE-2022-3171
  lazy val protobuf            = "com.google.protobuf"          % "protobuf-java"         % "3.23.0"
  // XXX Bundled version is vulnerable to CVE-2023-34455
  lazy val snappyJava          = "org.xerial.snappy"            % "snappy-java"           % "1.1.10.1"
}

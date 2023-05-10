import sbt._

object Dependencies {
  // Runtime
  // XXX json4s cannot currently be upgrade to maintain compatibility with Spark
  lazy val json4s              = "org.json4s"                 %% "json4s-jackson"        % "3.7.0-M11"
  lazy val json4sScalaz        = "org.json4s"                 %% "json4s-scalaz"         % "3.7.0-M11"

  lazy val bloomFilter         = "com.sangupta"                % "bloomfilter"           % "0.9.1-SNAPSHOT"
  lazy val dbscan              = "com.github.chrfrantz"        % "DBSCAN"                % "1de90e7"
  lazy val ddSketch            = "com.datadoghq"               % "sketches-java"         % "0.8.2"
  lazy val fuzzySets           = "com.github.dataunitylab"    %% "fuzzy-sets"            % "0.2.0-1-g6b2baf2-SNAPSHOT"
  lazy val hyperLogLog         = "com.github.prasanthj"        % "hyperloglog"           % "1.1"
  lazy val jsonSchemaValidator = "com.networknt"               % "json-schema-validator" % "1.0.79"
  lazy val openLocationCode    = "com.google.openlocationcode" % "openlocationcode"      % "1.0.4"
  lazy val scopt               = "com.github.scopt"           %% "scopt"                 % "4.1.0"
  lazy val scalaCsv            = "com.github.tototoshi"       %% "scala-csv"             % "1.3.10"
  lazy val spark               = "org.apache.spark"           %% "spark-core"            % "3.4.0"
  lazy val sparkSql            = "org.apache.spark"           %% "spark-sql"             % "3.4.0"
  lazy val validator           = "commons-validator"           % "commons-validator"     % "1.7"

  // Test
  lazy val scalactic           = "org.scalactic"              %% "scalactic"              % "3.2.15"
  lazy val scalaTest           = "org.scalatest"              %% "scalatest"              % "3.2.15"

  // Overrides
  // XXX This is necessary for Spark version consistency
  lazy val jacksonDatabind     = "com.fasterxml.jackson.core" %  "jackson-databind"       % "2.14.3"
  // XXX Version bundled with Spark is vulnerable to CVE-2022-3171
  lazy val protobuf            = "com.google.protobuf"          % "protobuf-java"         % "3.23.0"
}

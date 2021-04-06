import sbt._

object Dependencies {
  lazy val json4s              = "org.json4s"       %% "json4s-jackson"         % "3.7.0-M15"
  lazy val json4sScalaz        = "org.json4s"       %% "json4s-scalaz"          % "3.7.0-M15"
  lazy val jsonSchemaValidator = "com.networknt"    %  "json-schema-validator"  % "1.0.51"
  lazy val spark               = "org.apache.spark" %% "spark-core"             % "3.1.1"

  lazy val scalaTest           = "org.scalatest" %% "scalatest"              % "3.2.7"
}

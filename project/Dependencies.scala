import sbt._

object Dependencies {
  lazy val json4s              = "org.json4s"    %% "json4s-jackson"         % "3.7.0-M11"
  lazy val json4sScalaz        = "org.json4s"    %% "json4s-scalaz"          % "3.7.0-M11"
  lazy val jsonSchemaValidator = "com.networknt" %  "json-schema-validator"  % "1.0.51"

  lazy val scalaTest           = "org.scalatest" %% "scalatest"              % "3.2.2"
}

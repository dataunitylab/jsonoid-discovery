import Dependencies._

ThisBuild / scalaVersion      := "2.13.4"
ThisBuild / version           := "0.1.0-SNAPSHOT"
ThisBuild / organization      := "com.example"
ThisBuild / organizationName  := "example"
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

lazy val root = (project in file("."))
  .settings(
    name := "JSONoid Discovery",
    libraryDependencies ++= Seq(
        // Scala
        json4s,
        json4sScalaz,

        scalaTest % Test
    ),
    scalacOptions += "-Xlint:unused"
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

import Dependencies._

ThisBuild / scalaVersion      := "2.13.4"
ThisBuild / version           := "0.1.0-SNAPSHOT"
ThisBuild / organization      := "edu.rit.cs"
ThisBuild / organizationName  := "Rochester Institute of Technology"

lazy val root = (project in file("."))
  .settings(
    name := "JSONoid Discovery",
    libraryDependencies ++= Seq(
        // Scala
        json4s,
        json4sScalaz,
        jsonSchemaValidator,

        scalaTest % Test
    )
  )

enablePlugins(GhpagesPlugin)
enablePlugins(SiteScaladocPlugin)

git.remoteRepo := "git@github.com:michaelmior/jsonoid-discovery.git"

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

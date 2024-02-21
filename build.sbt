import Dependencies._
import com.typesafe.sbt.packager.docker._

ThisBuild / scalaVersion      := "2.13.10"
ThisBuild / versionScheme     := Some("early-semver")
ThisBuild / organization      := "io.github.dataunitylab"
ThisBuild / organizationName  := "Rochester Institute of Technology"
ThisBuild / githubOwner       := "dataunitylab"
ThisBuild / githubRepository  := "jsonoid-discovery"

inThisBuild(
  List(
    organization := "io.github.dataunitylab",
    homepage := Some(url("https://github.com/dataunitylab/jsonoid-discovery")),
    licenses := List("MIT" -> url("http://opensource.org/licenses/MIT")),
    developers := List(
      Developer(
        "michaelmior",
        "Michael Mior",
        "mmior@mail.rit.edu ",
        url("https://michael.mior.ca")
      )
    ),
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
)

scalafixOnCompile := true
ThisBuild / scalafixDependencies += "net.pixiv" %% "scalafix-pixiv-rule" % "3.0.1"

mainClass := Some("io.github.dataunitylab.jsonoid.discovery.DiscoverSchema")

Global / onChangedBuildSource := ReloadOnSourceChanges

val nonConsoleCompilerOptions = Seq(
  "-feature",
  "-Xfatal-warnings",
  "-Ywarn-unused:imports",
  "-deprecation",
  "-release:8"
) ++ {if (sys.env.get("DISABLE_ASSERTIONS").isDefined)
  Seq("-Xdisable-assertions")
else
  Nil
}

val generateSchemas = taskKey[Unit]("Generate example schemas")

generateSchemas := {
  import sbt.Attributed.data

  import java.nio.file.{FileSystems, Files}

  val r = (Compile / runner).value
  val cp = (Compile / fullClasspath).value

  // Generate the output directory to store generated schemas
  val schemaPath = FileSystems.getDefault().getPath("target", "jsonoid-schemas")
  Files.createDirectories(schemaPath)

  val inputs = List(
    "earthquakes.json",
    "gdp.json",
    "mr-robot.json",
    "nobel.json",
    "rickandmorty.json",
    "test.json",
    "jsonlines-example.json"
  )

  for (input <- inputs) {
    r.run("io.github.dataunitylab.jsonoid.discovery.DiscoverSchema",
    data(cp),
    Seq(
      "src/test/resources/" + input,
      "-p",
      "Simple",
      "-w",
      schemaPath.resolve(input).toString
    ),
    (streams.value: @sbtUnchecked).log)
  }
}

lazy val root = (project in file("."))
  .settings(
    name := "JSONoid Discovery",

    // For bloomfilter
    resolvers += Resolver.githubPackages("michaelmior"),

    // For DBSCAN
    resolvers += "jitpack" at "https://jitpack.io",

    libraryDependencies ++= Seq(
        bloomFilter,
        ddSketch,
        json4s,
        json4sScalaz,
        dbscan,
        fuzzySets,
        hyperLogLog,
        jsonSchemaValidator,
        openLocationCode,
        scopt,
        scalaCsv,
        validator,

        spark % "provided",
        sparkSql % "provided",

        scalactic % Test,
        scalaTest % Test,
        scalaTestPlus % Test,
        scalaCheck % Test,
    ),
    dependencyOverrides ++= Seq(
      guava,
      jacksonDatabind,
      protobuf,
      snappyJava,
    ),
    javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
    scalacOptions ++= nonConsoleCompilerOptions,
    buildInfoKeys := Seq[BuildInfoKey](version),
    buildInfoPackage := "io.github.dataunitylab.jsonoid.discovery"
  )

lazy val fuzz = (project in file("fuzz")).settings(
   libraryDependencies ++= Seq(
     jazzer,
     json4s
   )
).dependsOn(root)

Compile / compile / wartremoverErrors ++= Seq(
  Wart.ArrayEquals,
  Wart.EitherProjectionPartial,
  Wart.Enumeration,
  Wart.Equals,
  Wart.ExplicitImplicitTypes,
  Wart.FinalCaseClass,
  Wart.FinalVal,
  Wart.JavaConversions,
  Wart.JavaSerializable,
  Wart.LeakingSealed,
  Wart.MutableDataStructures,
  Wart.NonUnitStatements,
  Wart.Null,
  Wart.Option2Iterable,
  Wart.OptionPartial,
  Wart.Product,
  Wart.PublicInference,
  Wart.Recursion,
  Wart.Return,
  Wart.Serializable,
  Wart.StringPlusAny,
  Wart.ToString,
  Wart.TripleQuestionMark,
  Wart.TryPartial,
  Wart.Var,
  Wart.While,
)

Compile / console / scalacOptions := (console / scalacOptions)
  .value.filterNot(opt =>
    nonConsoleCompilerOptions.contains(opt)
)

enablePlugins(BuildInfoPlugin)
enablePlugins(DockerPlugin)
enablePlugins(GitHubPagesPlugin)
enablePlugins(JavaAppPackaging)
enablePlugins(SiteScaladocPlugin)

dockerEntrypoint := Seq("/opt/docker/bin/discover-schema")
dockerBaseImage := "openjdk:8-alpine"
dockerCommands ++= Seq(
  Cmd("USER", "root"),
  ExecCmd("RUN", "apk", "add", "--no-cache", "bash"),
)

gitHubPagesOrgName := "dataunitylab"
gitHubPagesRepoName := "jsonoid-discovery"
gitHubPagesSiteDir := baseDirectory.value / "target/site"

git.remoteRepo := "git@github.com:dataunitylab/jsonoid-discovery.git"
git.useGitDescribe := true

Test / fork := true

assembly / assemblyMergeStrategy := {
  case "module-info.class" => MergeStrategy.discard
  case "META-INF/versions/9/module-info.class" => MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}
fuzz / assembly / assemblyMergeStrategy := (assembly / assemblyMergeStrategy).value
assembly / assemblyJarName        := s"jsonoid-discovery-${version.value}.jar"
fuzz / assembly / assemblyJarName := "fuzz.jar"

import sbtassembly.AssemblyPlugin.defaultUniversalScript
assemblyPrependShellScript := Some(defaultUniversalScript(shebang = false))

run / connectInput := true

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

apiMappings ++= {
  def mappingsFor(organization: String, names: List[String], location: String, revision: (String) => String = identity): Seq[(File, URL)] =
    for {
      entry: Attributed[File] <- (Compile / fullClasspath).value
      module: ModuleID <- entry.get(moduleID.key)
      if module.organization == organization
      if names.exists(module.name.startsWith)
    } yield entry.data -> url(location.format(revision(module.revision)))

  val mappings: Seq[(File, URL)] =
    mappingsFor("org.scala-lang", List("scala-library"), "http://scala-lang.org/api/%s/")

  mappings.toMap
}

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
sonatypeRepository := "https://s01.oss.sonatype.org/service/local"
val releaseToSonatype = sys.env.getOrElse("RELEASE_SONATYPE", "false").toBoolean
publishTo := {if (releaseToSonatype) sonatypePublishTo.value else githubPublishTo.value}
ThisBuild / dynverSonatypeSnapshots := releaseToSonatype
ThisBuild / dynverSeparator := "-"

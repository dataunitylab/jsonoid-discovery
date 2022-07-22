import Dependencies._

ThisBuild / scalaVersion      := "2.11.12"
ThisBuild / versionScheme     := Some("early-semver")
ThisBuild / organization      := "edu.rit.cs"
ThisBuild / organizationName  := "Rochester Institute of Technology"
ThisBuild / githubOwner       := "michaelmior"
ThisBuild / githubRepository  := "jsonoid-discovery"

mainClass := Some("edu.rit.cs.mmior.jsonoid.discovery.DiscoverSchema")

Global / onChangedBuildSource := ReloadOnSourceChanges

val nonConsoleCompilerOptions = Seq(
  "-feature",
  "-Xfatal-warnings",
  "-Ywarn-unused-import",
  "-deprecation"
)

lazy val core = (project in file("core"))
  .settings(
    name := "JSONoid Discovery",
    resolvers += Resolver.githubPackages("michaelmior"),
    resolvers += "jitpack" at "https://jitpack.io",
    libraryDependencies ++= Seq(
        bloomFilter,
        json4s,
        json4sScalaz,
        dbscan,
        fuzzySets,
        hyperLogLog,
        jsonSchemaValidator,
        scopt,
        scalaCsv,

        spark % "provided",
        sparkSql % "provided",

        scalaTest % Test,
    ),
    dependencyOverrides ++= Seq(
      jacksonDatabind,
    ),
    scalacOptions ++= nonConsoleCompilerOptions,
    buildInfoKeys := Seq[BuildInfoKey](version),
    buildInfoPackage := "edu.rit.cs.mmior.jsonoid.discovery",

    gitHubPagesOrgName := "michaelmior",
    gitHubPagesRepoName := "jsonoid-discovery",
    gitHubPagesSiteDir := baseDirectory.value / "core/target/site",

    git.remoteRepo := "git@github.com:michaelmior/jsonoid-discovery.git",
    git.useGitDescribe := true
  )
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(GitHubPagesPlugin)
  .enablePlugins(GitVersioning)
  .enablePlugins(SiteScaladocPlugin)

Compile / compile / wartremoverErrors ++= Seq(
  Wart.ArrayEquals,
  Wart.EitherProjectionPartial,
  Wart.Enumeration,
  Wart.Equals,
  Wart.ExplicitImplicitTypes,
  Wart.FinalCaseClass,
  Wart.MutableDataStructures,
  Wart.NonUnitStatements,
  Wart.Null,
  Wart.Option2Iterable,
  Wart.OptionPartial,
  Wart.PublicInference,
  Wart.Recursion,
  Wart.Return,
  Wart.StringPlusAny,
  Wart.TraversableOps,
  Wart.TryPartial,
  Wart.Var,
  Wart.While,
)

Compile / console / scalacOptions := (console / scalacOptions)
  .value.filterNot(opt =>
    nonConsoleCompilerOptions.contains(opt)
)

Test / fork := true

assembly / assemblyMergeStrategy := {
  case "module-info.class" => MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}
assembly / assemblyJarName       := s"jsonoid-discovery-${version.value}.jar"

import sbtassembly.AssemblyPlugin.defaultUniversalScript
assemblyPrependShellScript := Some(defaultUniversalScript(shebang = false))

run / connectInput := true

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

sonatypeProfileName := "io.github.dataunitylab"
publishMavenStyle := true
licenses := Seq("MIT" -> url("https://raw.githubusercontent.com/dataunitylab/jsonoid-discovery/main/LICENSE.md"))

import xerial.sbt.Sonatype._
sonatypeProjectHosting := Some(GitHubHosting("dataunitylab", "jsonoid-discovery", "mmior@mail.rit.edu"))

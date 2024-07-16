scalaVersion := "2.13.16"
scalacOptions += "-language:higherKinds"
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.3" cross CrossVersion.full)

scalacOptions += "-Ydelambdafy:inline"
scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-language:reflectiveCalls",
  "-Ymacro-annotations"
)
val chiselVersion = "7.0.0-M2+639-5df5515f-SNAPSHOT"
addCompilerPlugin("org.chipsalliance" %% "chisel-plugin" % chiselVersion cross CrossVersion.full)
libraryDependencies ++= Seq(
  "org.chipsalliance" %% "chisel" % chiselVersion,
  "org.scalatest" %% "scalatest" % "3.2.15" % "test",
  "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % "test",
)
resolvers ++= Resolver.sonatypeOssRepos("snapshots")

Test / parallelExecution := false

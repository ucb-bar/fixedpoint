scalaVersion := "2.13.18"
scalacOptions += "-language:higherKinds"
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.4" cross CrossVersion.full)

scalacOptions += "-Ydelambdafy:inline"
scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-language:reflectiveCalls",
  "-Ymacro-annotations"
)
val chiselVersion = "7.5.0"
addCompilerPlugin("org.chipsalliance" %% "chisel-plugin" % chiselVersion cross CrossVersion.full)
libraryDependencies ++= Seq(
  "org.chipsalliance" %% "chisel"          % chiselVersion,
  "org.scalatest"     %% "scalatest"       % "3.2.19"   % "test",
  "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % "test"
)

Test / parallelExecution := false

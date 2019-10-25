lazy val root = (project in file("."))
  .settings(
    name := "scala-nunjucks",
    version := "0.1.0",
    scalaVersion := "2.11.12",
    scalacOptions += "-Ypartial-unification",
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % "2.1.2",
      "org.typelevel" %% "cats-core" % "1.6.1",
      "com.github.mpilquist" %% "simulacrum" % "0.19.0",
      "org.scalactic" %% "scalactic" % "3.0.8",
      "org.scalatest" %% "scalatest" % "3.0.8" % Test,
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
    )
  )
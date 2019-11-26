import play.core.PlayVersion

lazy val lib = (project in file("."))
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
      "org.webjars.npm" % "govuk-frontend" % "3.1.0" % Test
    )
  )

lazy val playTest = (project in file("play-test"))
  .enablePlugins(PlayScala)
  .dependsOn(lib, playNunjucks)
  .settings(
    name := "play-test",
    scalaVersion := "2.11.12",
    libraryDependencies ++= Seq(
      filters,
      "com.typesafe.play"      %% "play-guice"          % PlayVersion.current,
      "org.webjars.npm"        %  "govuk-frontend"      % "3.1.0",
      "org.scalactic"          %% "scalactic"           % "3.0.7"             % "test",
      "org.scalatest"          %% "scalatest"           % "3.0.7"             % "test",
      "org.scalacheck"         %% "scalacheck"          % "1.14.0"            % "test",
      "org.pegdown"            %  "pegdown"             % "1.6.0"             % "test"
    ),
    Concat.groups := Seq(
      "javascripts/application.js" -> group(Seq("lib/govuk-frontend/govuk/all.js"))
    ),
    uglifyCompressOptions := Seq("unused=false", "dead_code=false"),
    pipelineStages in Assets := Seq(concat,uglify)
  )

lazy val playNunjucks = (project in file("play-nunjucks"))
  .dependsOn(lib)
  .settings(
    name := "play-nunjucks",
    version := "0.1.0",
    scalaVersion := "2.11.12",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.8" % Test,
      "com.typesafe.play" %% "play" % PlayVersion.current,
      "com.typesafe.play" %% "play-test" % PlayVersion.current,
      "com.github.pathikrit" %% "better-files" % "3.5.0"
    )
  )

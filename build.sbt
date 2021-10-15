
lazy val root = project
  .in(file("."))
  .settings(
    name := "TraceChecker",
    version := "0.1",
    organization := "ca.ubc.cs",
    scalaVersion := "2.13.6",

    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "io.spray" %%  "spray-json" % "1.3.6",
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.7.8",
    libraryDependencies += "com.lihaoyi" %% "fansi" % "0.2.14",
    libraryDependencies += "com.lihaoyi" %% "pprint" % "0.6.6",
    libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.2.7",

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test,
    libraryDependencies += "io.github.java-diff-utils" % "java-diff-utils" % "4.11" % Test,
  )


lazy val root = project
  .in(file("."))
  .settings(
    name := "tracechecker",
    version := "0.1.0-SNAPSHOT",
    organization := "com.github.distributedclocks",
    scalaVersion := "2.13.7",

    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "io.spray" %%  "spray-json" % "1.3.6",
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.8.0",
    libraryDependencies += "com.lihaoyi" %% "fansi" % "0.3.0",
    libraryDependencies += "com.lihaoyi" %% "pprint" % "0.7.1",
    libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.2.7",

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test,
    libraryDependencies += "io.github.java-diff-utils" % "java-diff-utils" % "4.11" % Test,
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4" % Test
  )

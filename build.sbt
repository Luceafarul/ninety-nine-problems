val scala3Version = "3.2.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "ninety-nine-problems",
    version := "0.0.1",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )

name := "ali"
version := "0.0.1"

scalaVersion := "2.12.6"

val circeVersion = "0.9.3"

val deps = Seq(
  "org.typelevel" %% "cats-core" % "1.1.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",

  "com.github.pureconfig" %% "pureconfig" % "0.9.1",
  "com.typesafe.akka" %% "akka-http" % "10.1.1",
  "com.typesafe.akka" %% "akka-stream" % "2.5.11",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion)

val testDeps = Seq(
  "org.scalatest" %% "scalatest" % "3.0.1",
  "com.typesafe.akka" %% "akka-http-testkit" % "10.1.1"
).map(_ % "test")

libraryDependencies ++= deps ++ testDeps

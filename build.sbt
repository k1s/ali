name := "ali"
version := "0.0.1"

scalaVersion := "2.12.6"

scalacOptions += "-Ypartial-unification"

val deps = Seq(
  "org.typelevel" %% "cats-core" % "1.5.0",
  "org.typelevel" %% "cats-kernel" % "1.5.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0"
)

val testDeps = Seq(
  "org.scalatest" %% "scalatest" % "3.0.1"
).map(_ % "test")

libraryDependencies ++= deps ++ testDeps

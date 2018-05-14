lazy val root = project
  .in(file("."))
  .settings(
    name := "hexlet",
    version := "0.0.1",

    scalaVersion := "2.12.6",

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "1.1.0",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0"
    ) ++ Seq(
      "org.scalatest" %% "scalatest" % "3.0.1"
    ).map(_ % "test")
  )
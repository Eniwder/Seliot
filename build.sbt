name := "Seliot"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5",
  "org.scalaz" %% "scalaz-core" % "7.1.5",
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scala-lang" % "scala-swing" % "2.11+"
)


scalacOptions ++= Seq("-Yrangepos", "-unchecked", "-deprecation", "-feature")
name := "gdget-core"

version := "0.1"

scalaVersion := "2.11.7"

resolvers += Resolver.sonatypeRepo("releases")


libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
libraryDependencies += "org.typelevel" %% "cats" % "0.4.0"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.2.4"
libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.7"

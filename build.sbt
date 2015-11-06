name := "gdget-core"

version := "0.1"

scalaVersion := "2.11.7"

resolvers += Resolver.sonatypeRepo("releases")


libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.3"
libraryDependencies += "org.typelevel" %% "machinist" % "0.4.1"
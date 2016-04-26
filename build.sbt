name := "gdget"


def GdgetProject(name: String): Project = (
  Project(name, file(name)).
    settings(
      version := "0.1",
      organization := "org.gdget",
      scalaVersion := "2.11.8",
      resolvers ++= Seq(Resolver.sonatypeRepo("releases")),
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "2.2.6" % "test",
        "org.typelevel" %% "cats" % "0.4.1"
      )
    )
)


lazy val core = GdgetProject("core").settings()

val partitioned = GdgetProject("partitioned").dependsOn(core).settings()

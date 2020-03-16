name := "tic-cat-toe"

scalaVersion := "2.13.1"

libraryDependencies += "org.typelevel" %% "cats-effect" % "2.1.1" withSources() withJavadoc()
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"
libraryDependencies += "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.3" % "test"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Xlint",
  "-Xfatal-warnings",
)

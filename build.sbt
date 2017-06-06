name := "seminar-workshop"

version := "1.0"

scalaVersion := "2.12.2"

javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint")

libraryDependencies += "org.typelevel" %% "cats" % "0.9.0"
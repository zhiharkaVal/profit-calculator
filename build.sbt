ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"

lazy val root = (project in file("."))
  .settings(
    name := "profit-counter"
  )

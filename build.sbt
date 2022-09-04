ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "shapeless-exercise",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.9",
      "org.typelevel" %% "cats-core" % "2.8.0",
    )
  )

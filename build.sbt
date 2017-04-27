name := "protomacro"

scalaVersion in ThisBuild := "2.12.2"

scalacOptions in ThisBuild ++= Seq("-feature", "-deprecation")

lazy val root = (project in file("."))
  .dependsOn(macros)
  .settings(scalacOptions ++= Seq("-Xlog-implicits"))

lazy val macros = (project in file("macros"))
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

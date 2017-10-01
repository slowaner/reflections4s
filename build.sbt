import sbt.Keys._
import sbt._
import xerial.sbt.pack.PackPlugin

val organizationName = "org.slowaner"
val rootProjectName = "reflections4s"

// Versions
// Testing
val scalatestVersion = "3.0.3"
val junitVersion = "4.12"

// Reflections
val reflectionsVersion = "0.9.11"

// Logging
val slf4jVersion = "1.7.25"
val logbackVersion = "1.2.3"

lazy val commonSettings = Defaults.defaultConfigs ++ Seq(
  organization := organizationName,
  scalaVersion := "2.12.3",
  crossPaths := false,
  libraryDependencies ++= Seq(
    // Testing
    "org.scalatest" %% "scalatest" % scalatestVersion % Test,
    "junit" % "junit" % junitVersion % Test,
    // Logging
    "org.slf4j" % "slf4j-api" % slf4jVersion,
    "ch.qos.logback" % "logback-classic" % logbackVersion
  ),
  excludeFilter := new SimpleFileFilter(f => f.getName match {
    case ".gitignore" | ".gitkeep" => true
    case _ => false
  })
)

lazy val rootSettings = PackPlugin.packSettings ++ Seq(
  name := rootProjectName,
  libraryDependencies ++= Seq(
    // Scala
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    // Reflections
    "org.reflections" % "reflections" % reflectionsVersion
  )
)

lazy val root = Project(rootProjectName, file("."))
  .settings(commonSettings ++ rootSettings)
  .enablePlugins(PackPlugin)

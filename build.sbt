name := "barbalion-lib"

version := "0.1"

organization := "com.barbalion"

organizationName := "Barbalion"

organizationHomepage := Some(new URL("http://barbalion.com"))

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
//  "org.scaloid" %% "scaloid" % "latest.release" withSources() withJavadoc(),
//  "io.reactivex" %% "rxscala" % "latest.release" withSources() withJavadoc(),
  "org.scalatest" %% "scalatest" % "latest.release" % "test" withSources() withJavadoc()
)

isSnapshot := true

exportJars := true

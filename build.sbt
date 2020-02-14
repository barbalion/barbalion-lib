name := "barbalion-lib"

version := "0.1"

organization := "com.barbalion"

organizationName := "Barbalion"

organizationHomepage := Some(new URL("http://barbalion.com"))

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
//  "org.scaloid" %% "scaloid" % "latest.release" withSources() withJavadoc(),
//  "io.reactivex" %% "rxscala" % "latest.release" withSources() withJavadoc(),
  "org.scalatest" %% "scalatest" % "3.1.0" % "test" withSources() withJavadoc()
)

//resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

isSnapshot := true

exportJars := true

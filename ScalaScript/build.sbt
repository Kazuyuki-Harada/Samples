organization := "sample"

name := "scala script"

version := "1.0v20120806"

scalaVersion := "2.9.2"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

licenses += "Apache License Version 2.0" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")

libraryDependencies += "org.scala-lang" % "scala-library" % "2.9.2"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.9.2"

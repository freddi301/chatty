name := "chatty"
version := "0.1"
scalaVersion := "3.1.0"

// scala js
enablePlugins(ScalaJSPlugin)
name := "Scala.js Tutorial"
scalaVersion := "3.1.0" // or any other Scala version >= 2.11.12
// This is an application with a main method
scalaJSUseMainModuleInitializer := true
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.0.0"

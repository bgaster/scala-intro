scalaJSSettings

name := "Scala.js Tutorial"

scalaVersion := "2.11.5"

libraryDependencies += "org.scala-lang.modules.scalajs" %%% "scalajs-dom" % "0.6"
ScalaJSKeys.jsDependencies += scala.scalajs.sbtplugin.RuntimeDOM

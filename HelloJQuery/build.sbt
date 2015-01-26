scalaJSSettings

name := "Scala.js Tutorial"

scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
  "org.scala-lang.modules.scalajs" %%% "scalajs-dom" % "0.6",
  "org.scala-lang.modules.scalajs" %%% "scalajs-jquery" % "0.6",
  "org.scala-lang.modules.scalajs" %% "scalajs-jasmine-test-framework" % scalaJSVersion % "test"
)

skip in ScalaJSKeys.packageJSDependencies := false

ScalaJSKeys.persistLauncher in Compile := true

lazy val root = (project in file(".")).
  settings(
    name              := "playground",
    version           := "1.0",
    scalaVersion      := "2.11.5",
    organization      := "com.github.bendict.gaster",
    mainClass in assembly := Some("com.example.Main")
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"
libraryDependencies += "com.googlecode.kiama" %% "kiama" % "1.8.0"

resolvers ++= Seq(
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.1.0"
)

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.1"

resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"

// for scala 2.11
addCompilerPlugin("org.spire-math" % "kind-projector_2.11" % "0.5.2")

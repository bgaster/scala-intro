lazy val root = (project in file(".")).
  settings(
    name              := "caesercipher",
    version           := "1.0",
    scalaVersion      := "2.11.5",
    organization      := "com.github.bendict.gaster",
    mainClass in assembly := Some("com.example.Main")
  )

libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"

resolvers += Resolver.sonatypeRepo("public")

lazy val root = (project in file(".")).
  settings(
    name              := "2dconsole",
    version           := "1.0",
    scalaVersion      := "2.11.5",
    organization      := "com.github.bendict.gaster",
    mainClass in assembly := Some("com.example.Main")
  )

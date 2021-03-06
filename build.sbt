
lazy val root = (project in file(".")).
  settings(
    name := "MizarParser",
    version := "0.1.0",
    scalaVersion := "2.11.7",
    scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation"),
    libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.0",

    libraryDependencies += "com.lihaoyi" % "ammonite" % "1.0.0" % "test" cross CrossVersion.full,

    sourceGenerators in Test += Def.task {
      val file = (sourceManaged in Test).value / "amm.scala"
      IO.write(file, """object amm extends App { ammonite.Main().run() }""")
      Seq(file)
    }.taskValue

  )

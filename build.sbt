
lazy val root = (project in file(".")).
  settings(
    name := "MizarParser",
    version := "1.0",
    scalaVersion := "2.10.4",
    scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation"),
    libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.0"
  )

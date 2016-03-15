
lazy val root = (project in file(".")).
  settings(
    name := "MizarParser",
    version := "0.1.0",
    scalaVersion := "2.11.7",
    scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation"),
    libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.0"
  )

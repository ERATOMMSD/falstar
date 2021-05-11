name := "falstar"

scalaVersion :="2.12.13"

libraryDependencies += "org.apache.commons" % "commons-csv" % "1.8"

mainClass in (Compile, run) := Some("falstar.Main")
mainClass in (Compile, packageBin) := Some("falstar.Main")
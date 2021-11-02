name := "trellosupport"

scalaVersion := "2.13.7"

libraryDependencies ++= Seq(
  "org.scalaj" %% "scalaj-http" % "2.4.2",
  "com.lihaoyi" %% "upickle" % "1.4.2",
  "dev.zio" %% "zio" % "2.0.0-M4"
)

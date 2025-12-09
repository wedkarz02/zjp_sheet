name := "sheet"
version := "0.0.1"

scalaVersion := "3.7.4"

Compile / scalacOptions ++= Seq(
  "-deprecation",
  "-explain",
  "-feature",
  "-print-lines",
  "-unchecked",
  "-Xfatal-warnings",
  "-source:3.7",
  "-encoding", "utf8"
)


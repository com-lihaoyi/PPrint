
lazy val pprintJVM = pprint.jvm
lazy val pprintJS = pprint.js
lazy val pprintNative = pprint.native
lazy val readme = scalatex.ScalatexReadme(
  projectId = "readme",
  wd = file(""),
  url = "https://github.com/lihaoyi/pprint/tree/master",
  source = "Readme"
).settings(
  scalaVersion := "2.11.12",
  (unmanagedSources in Compile) += baseDirectory.value/".."/"project"/"Constants.scala"
)

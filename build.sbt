
lazy val readme = scalatex.ScalatexReadme(
  projectId = "readme",
  wd = file(""),
  url = "https://github.com/lihaoyi/pprint/tree/master",
  source = "Readme"
).settings(
  scalaVersion := "2.11.12",
  ((Compile / unmanagedSources)) += baseDirectory.value/".."/"project"/"Constants.scala"
)

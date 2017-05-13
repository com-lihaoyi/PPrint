val baseSettings = Seq(
  organization := "com.lihaoyi",
  name := "tprint",
  version := "0.4.6",
  scalaVersion := "2.11.11",
  crossScalaVersions := Seq("2.10.6", "2.11.11", "2.12.2", "2.13.0-M1"),
  scmInfo := Some(ScmInfo(
    browseUrl = url("https://github.com/lihaoyi/TPrint"),
    connection = "scm:git:git@github.com:lihaoyi/TPrint.git"
  )),
  homepage := Some(url("https://github.com/lihaoyi/TPrint")),
  licenses := Seq("MIT" -> url("http://www.opensource.org/licenses/mit-license.html")),
  developers += Developer(
    email = "haoyi.sg@gmail.com",
    id = "lihaoyi",
    name = "Li Haoyi",
    url = url("https://github.com/lihaoyi")
  )
)

baseSettings

lazy val tprint = crossProject.crossType(CrossType.Pure)
  .settings(baseSettings)
  .settings(
    scalacOptions ++= Seq(scalaBinaryVersion.value match {
      case x if x.startsWith("2.12") => "-target:jvm-1.8"
      case _ => "-target:jvm-1.7"
    }),
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "fansi" % "0.2.3",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided,
      "com.lihaoyi" %%% "utest" % "0.4.7" % Test,
      "com.chuusai" %%% "shapeless" % "2.3.2" % Test
    ),
    testFrameworks := Seq(new TestFramework("utest.runner.Framework")),
    publishTo := Some("releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"),
    unmanagedSourceDirectories in Compile ++= {
      if (Set("2.11", "2.12", "2.13.0-M1").contains(scalaBinaryVersion.value))
        Seq(baseDirectory.value / ".." / "src" / "main" / "scala-2.10+")
      else
        Seq()
    },
    sourceGenerators in Compile += Def.task {
      val dir = (sourceManaged in Compile).value
      val file = dir/"pprint"/"TPrintGen.scala"

      val typeGen = for(i <- 2 to 22) yield {
        val ts = (1 to i).map("T" + _).mkString(", ")
        val tsBounded = (1 to i).map("T" + _ + ": Type").mkString(", ")
        val tsGet = (1 to i).map("get[T" + _ + "](cfg)").mkString(" + \", \" + ")
        s"""
          implicit def F${i}TPrint[$tsBounded, R: Type] = make[($ts) => R](cfg =>
            "(" + $tsGet + ") => " + get[R](cfg)
          )
          implicit def T${i}TPrint[$tsBounded] = make[($ts)](cfg =>
            "(" + $tsGet + ")"
          )
        """
      }
      val output = s"""
        package pprint
        trait TPrintGen[Type[_], Cfg]{
          def make[T](f: Cfg => String): Type[T]
          def get[T: Type](cfg: Cfg): String
          implicit def F0TPrint[R: Type] = make[() => R](cfg => "() => " + get[R](cfg))
          implicit def F1TPrint[T1: Type, R: Type] = {
            make[T1 => R](cfg => get[T1](cfg) + " => " + get[R](cfg))
          }
          ${typeGen.mkString("\n")}
        }
      """.stripMargin
      IO.write(file, output)
      Seq(file)
    }.taskValue
  )

lazy val tprintJVM = tprint.jvm
lazy val tprintJS = tprint.js

import mill._, scalalib._, scalajslib._, scalanativelib._, publish._
import $ivy.`com.lihaoyi::mill-contrib-bloop:$MILL_VERSION`
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version_mill0.9:0.1.1`
import de.tobiasroeser.mill.vcs.version.VcsVersion

val dottyVersions = sys.props.get("dottyVersion").toList

val scalaVersions = "2.12.13" :: "2.13.4" :: "2.11.12" :: "3.0.0-RC3" :: dottyVersions
val scala2Versions = scalaVersions.filter(_.startsWith("2."))

val scalaJSVersions = for {
  scalaV <- scalaVersions
  scalaJSV <- Seq("0.6.33", "1.4.0")
  if scalaV.startsWith("2.") || scalaJSV.startsWith("1.")
} yield (scalaV, scalaJSV)

val scalaNativeVersions = for {
  scalaV <- scala2Versions
  scalaNativeV <- Seq("0.4.0")
} yield (scalaV, scalaNativeV)

trait PPrintModule extends PublishModule {
  def artifactName = "pprint"

  def publishVersion = VcsVersion.vcsState().format()

  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "com.lihaoyi",
    url = "https://github.com/lihaoyi/PPrint",
    licenses = Seq(License.MIT),
    scm = SCM(
      "git://github.com/lihaoyi/PPrint.git",
      "scm:git://github.com/lihaoyi/PPrint.git"
    ),
    developers = Seq(
      Developer("lihaoyi", "Li Haoyi", "https://github.com/lihaoyi")
    )
  )
}
trait PPrintMainModule extends CrossScalaModule {
  def millSourcePath = super.millSourcePath / offset
  def ivyDeps = Agg(
    ivy"com.lihaoyi::fansi::0.2.13",
    ivy"com.lihaoyi::sourcecode::0.2.6"
  )
  def compileIvyDeps =
    if (crossScalaVersion.startsWith("2")) Agg(
      ivy"${scalaOrganization()}:scala-reflect:${scalaVersion()}",
      ivy"${scalaOrganization()}:scala-compiler:${scalaVersion()}"
    )
    else Agg.empty[Dep]
  def offset: os.RelPath = os.rel
  def sources = T.sources(
    super.sources()
      .flatMap(source =>
        Seq(
          PathRef(source.path / os.up / source.path.last),
          PathRef(source.path / os.up / os.up / source.path.last),
        )
      )
  )
  def generatedSources = T{
    val dir = T.ctx().dest
    val file = dir/"pprint"/"TPrintGen.scala"

    val typeGen = for(i <- 2 to 22) yield {
      val ts = (1 to i).map("T" + _).mkString(", ")
      val tsBounded = (1 to i).map("T" + _ + ": Type").mkString(", ")
      val tsGet = (1 to i).map("get[T" + _ + "](cfg)").mkString(" + \", \" + ")
      s"""
          implicit def F${i}TPrint[$tsBounded, R: Type]: Type[($ts) => R] = make[($ts) => R](cfg =>
            "(" + $tsGet + ") => " + get[R](cfg)
          )
          implicit def T${i}TPrint[$tsBounded]: Type[($ts)] = make[($ts)](cfg =>
            "(" + $tsGet + ")"
          )
        """
    }
    val output = s"""
        package pprint
        trait TPrintGen[Type[_], Cfg]{
          def make[T](f: Cfg => String): Type[T]
          def get[T: Type](cfg: Cfg): String
          implicit def F0TPrint[R: Type]: Type[() => R] = make[() => R](cfg => "() => " + get[R](cfg))
          implicit def F1TPrint[T1: Type, R: Type]: Type[T1 => R] = {
            make[T1 => R](cfg => get[T1](cfg) + " => " + get[R](cfg))
          }
          ${typeGen.mkString("\n")}
        }
      """.stripMargin
    os.write(file, output, createFolders = true)
    Seq(PathRef(file))
  }

}


trait PPrintTestModule extends ScalaModule with TestModule {
  def crossScalaVersion: String
  def testFrameworks = Seq("utest.runner.Framework")
  def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.7.9")
  def offset: os.RelPath = os.rel
  def millSourcePath = super.millSourcePath / os.up

  def sources = T.sources(
    super.sources()
      .++(CrossModuleBase.scalaVersionPaths(crossScalaVersion, s => millSourcePath / s"src-$s" ))
      .flatMap(source =>
        Seq(
          PathRef(source.path / os.up / "test" / source.path.last),
          PathRef(source.path / os.up / os.up / "test" / source.path.last),
        )
      )
      .distinct
  )
}

object pprint extends Module {
  object jvm extends Cross[JvmPPrintModule](scalaVersions:_*)
  class JvmPPrintModule(val crossScalaVersion: String)
    extends PPrintMainModule with ScalaModule with PPrintModule {
    object test extends Tests with PPrintTestModule{
      val crossScalaVersion = JvmPPrintModule.this.crossScalaVersion
    }
  }

  object js extends Cross[JsPPrintModule](scalaJSVersions:_*)
  class JsPPrintModule(val crossScalaVersion: String, crossJSVersion: String)
    extends PPrintMainModule with ScalaJSModule with PPrintModule {
    def offset = os.up
    def scalaJSVersion = crossJSVersion
    object test extends Tests with PPrintTestModule{
      def offset = os.up
      val crossScalaVersion = JsPPrintModule.this.crossScalaVersion
    }
  }

  object native extends Cross[NativePPrintModule](scalaNativeVersions:_*)
  class NativePPrintModule(val crossScalaVersion: String, crossScalaNativeVersion: String)
    extends PPrintMainModule with ScalaNativeModule with PPrintModule {
    def offset = os.up
    def scalaNativeVersion = crossScalaNativeVersion
    object test extends Tests with PPrintTestModule{
      def offset = os.up
      val crossScalaVersion = NativePPrintModule.this.crossScalaVersion
    }
  }

}

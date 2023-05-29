import mill._, scalalib._, scalajslib._, scalanativelib._, publish._
import mill.scalalib.api.Util.isScala3
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.1.4`
import de.tobiasroeser.mill.vcs.version.VcsVersion
import $ivy.`com.github.lolgab::mill-mima::0.0.21`
import com.github.lolgab.mill.mima._

val dottyCommunityBuildVersion = sys.props.get("dottyVersion").toList

val scalaVersions =
  "2.12.16" :: "2.13.8" :: "2.11.12" :: "3.1.3" :: dottyCommunityBuildVersion

val scalaJSVersions = scalaVersions.map((_, "1.10.1"))
val scalaNativeVersions = scalaVersions.map((_, "0.4.5"))

trait PPrintModule extends PublishModule with Mima {
  def artifactName = "pprint"

  def publishVersion = VcsVersion.vcsState().format()

  def mimaPreviousVersions = Seq("0.7.3", "0.8.0") ++ VcsVersion.vcsState().lastTag.toSeq

  def crossScalaVersion: String

  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "com.lihaoyi",
    url = "https://github.com/com-lihaoyi/PPrint",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github(
      owner = "com-lihaoyi",
      repo = "PPrint"
    ),
    developers = Seq(
      Developer("lihaoyi", "Li Haoyi", "https://github.com/lihaoyi")
    )
  )
}
trait PPrintMainModule extends CrossScalaModule {
  def millSourcePath = super.millSourcePath / offset
  def ivyDeps = Agg(
    ivy"com.lihaoyi::fansi::0.4.0",
    ivy"com.lihaoyi::sourcecode::0.3.0"
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
}


trait PPrintTestModule extends ScalaModule with TestModule.Utest {
  def crossScalaVersion: String
  def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.8.0")
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

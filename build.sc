import mill._, scalalib._, scalajslib._, scalanativelib._, publish._
import mill.scalalib.api.Util.isScala3
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.1.4`
import de.tobiasroeser.mill.vcs.version.VcsVersion
import $ivy.`com.github.lolgab::mill-mima::0.0.9`
import com.github.lolgab.mill.mima._

val dottyVersions = sys.props.get("dottyVersion").toList

val scala2VersionsAndDotty = "2.12.13" :: "2.13.4" :: "2.11.12" :: dottyVersions
val scala30 = "3.0.2"
val scala31 = "3.1.1"

val scalaJSVersions = for {
  scalaV <- scala30 :: scala2VersionsAndDotty
  scalaJSV <- Seq("0.6.33", "1.5.1")
  if scalaV.startsWith("2.") || scalaJSV.startsWith("1.")
} yield (scalaV, scalaJSV)

val scalaNativeVersions = for {
  scalaV <- scala31 :: scala2VersionsAndDotty
  scalaNativeV <- Seq("0.4.4")
} yield (scalaV, scalaNativeV)

trait PPrintModule extends PublishModule with Mima {
  def artifactName = "pprint"

  def publishVersion = VcsVersion.vcsState().format()

  def mimaPreviousVersions = VcsVersion.vcsState().lastTag.toSeq

  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "com.lihaoyi",
    url = "https://github.com/lihaoyi/PPrint",
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
    ivy"com.lihaoyi::fansi::0.3.1",
    ivy"com.lihaoyi::sourcecode::0.2.8"
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
  def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.7.11")
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
  object jvm extends Cross[JvmPPrintModule](scala30 :: scala2VersionsAndDotty:_*)
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
    // Remove after Scala Native Scala 3 artifacts are published
    def mimaPreviousArtifacts = T{ if(isScala3(scalaVersion())) Seq() else super.mimaPreviousArtifacts() }
    object test extends Tests with PPrintTestModule{
      def offset = os.up
      val crossScalaVersion = NativePPrintModule.this.crossScalaVersion
    }
  }

}

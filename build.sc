import mill._, scalalib._, scalajslib._, scalanativelib._, publish._
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.3.1-8-37c08a`
import $ivy.`com.github.lolgab::mill-mima::0.0.21`
import de.tobiasroeser.mill.vcs.version.VcsVersion
import com.github.lolgab.mill.mima._

val dottyCommunityBuildVersion = sys.props.get("dottyVersion").toList

val scalaVersions =
  Seq("2.12.16", "2.13.8", "2.11.12", "3.1.3") ++ dottyCommunityBuildVersion

trait PPrintModule
  extends CrossScalaModule with PublishModule with PlatformScalaModule with Mima {
  def publishVersion = VcsVersion.vcsState().format()

  def mimaPreviousVersions = Seq("0.7.3", "0.8.0") ++ VcsVersion.vcsState().lastTag.toSeq

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
}


trait PPrintTestModule extends ScalaModule with TestModule.Utest {
  def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.8.0")
}

object pprint extends Module {
  object jvm extends Cross[JvmPPrintModule](scalaVersions)
  trait JvmPPrintModule extends PPrintModule{
    object test extends Tests with PPrintTestModule
  }

  object js extends Cross[JsPPrintModule](scalaVersions)
  trait JsPPrintModule extends PPrintModule with ScalaJSModule {
    def scalaJSVersion = "1.10.1"
    object test extends Tests with PPrintTestModule
  }

  object native extends Cross[NativePPrintModule](scalaVersions)
  trait NativePPrintModule extends PPrintModule with ScalaNativeModule {
    def scalaNativeVersion = "0.4.5"
    object test extends Tests with PPrintTestModule
  }
}

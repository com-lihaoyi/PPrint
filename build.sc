import mill._, scalalib._, scalajslib._, scalanativelib._, publish._
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.4.0`
import $ivy.`com.github.lolgab::mill-mima::0.0.23`
import de.tobiasroeser.mill.vcs.version.VcsVersion
import com.github.lolgab.mill.mima._

val dottyCommunityBuildVersion = sys.props.get("dottyVersion").toList

val scalaVersions =
  Seq("2.12.16", "2.13.8", "3.3.1") ++ dottyCommunityBuildVersion

trait PPrintModule
  extends CrossScalaModule with PublishModule with PlatformScalaModule with Mima {
  def publishVersion = VcsVersion.vcsState().format()

  def mimaPreviousVersions = Seq("0.7.3", "0.8.0") ++ VcsVersion.vcsState().lastTag.toSeq

  def mimaReportBinaryIssues() =
    if (this.isInstanceOf[ScalaNativeModule] || this.isInstanceOf[ScalaJSModule]) T.command()
    else super.mimaReportBinaryIssues()
    
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
    ivy"com.lihaoyi::fansi::0.5.0",
    ivy"com.lihaoyi::sourcecode::0.4.0"
  )

  def compileIvyDeps = Agg.when(crossScalaVersion.startsWith("2"))(
    ivy"${scalaOrganization()}:scala-reflect:${scalaVersion()}",
    ivy"${scalaOrganization()}:scala-compiler:${scalaVersion()}"
  )
}


trait PPrintTestModule extends ScalaModule with TestModule.Utest {
  def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.8.4")
}

object pprint extends Module {
  object jvm extends Cross[JvmPPrintModule](scalaVersions)
  trait JvmPPrintModule extends PPrintModule{
    object test extends ScalaTests with PPrintTestModule
  }

  object js extends Cross[JsPPrintModule](scalaVersions)
  trait JsPPrintModule extends PPrintModule with ScalaJSModule {
    def scalaJSVersion = "1.12.0"
    object test extends ScalaJSTests with PPrintTestModule
  }

  object native extends Cross[NativePPrintModule](scalaVersions)
  trait NativePPrintModule extends PPrintModule with ScalaNativeModule {
    def scalaNativeVersion = "0.5.0"
    object test extends ScalaNativeTests with PPrintTestModule
  }
}

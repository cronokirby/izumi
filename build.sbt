import D._
import com.github.pshirshov.izumi.sbt.ConvenienceTasksPlugin.Keys.defaultStubPackage
import com.github.pshirshov.izumi.sbt.IzumiScopesPlugin.ProjectReferenceEx
import com.github.pshirshov.izumi.sbt.IzumiSettingsGroups.autoImport.SettingsGroupId._
import sbt.Keys.{pomExtra, publishMavenStyle}
import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._


enablePlugins(IzumiEnvironmentPlugin)
enablePlugins(IzumiDslPlugin)
enablePlugins(GitStampPlugin)

name := "izumi-r2"

val AppSettings = SettingsGroupId()
val LibSettings = SettingsGroupId()

val scala_212 = "2.12.4"
val scala_213 = "2.13.0-M2"

scalacOptions in ThisBuild ++= CompilerOptionsPlugin.dynamicSettings(scalaVersion.value, isSnapshot.value)
defaultStubPackage := Some("com.github.pshirshov.izumi")

val baseSettings = new GlobalSettings {
  override protected val settings: Map[SettingsGroupId, ProjectSettings] = Map(
    GlobalSettingsGroup -> new ProjectSettings {
      override val settings: Seq[sbt.Setting[_]] = Seq(
        organization := "com.github.pshirshov.izumi.r2"
        , crossScalaVersions := Seq(
          scala_212
        )
        , publishMavenStyle in Global := true
        , sonatypeProfileName := "com.github.pshirshov"
        , publishTo := Some(
          if (isSnapshot.value)
            Opts.resolver.sonatypeSnapshots
          else
            Opts.resolver.sonatypeStaging
        )
        , credentials in Global += Credentials(new File("credentials.sonatype-nexus.properties"))
        , pomExtra in Global := <url>https://bitbucket.org/pshirshov/izumi-r2</url>
          <licenses>
            <license>
              <name>BSD-style</name>
              <url>http://www.opensource.org/licenses/bsd-license.php</url>
              <distribution>repo</distribution>
            </license>
          </licenses>
          <developers>
            <developer>
              <id>pshirshov</id>
              <name>Pavel Shirshov</name>
              <url>https://github.com/pshirshov</url>
            </developer>
          </developers>

        , releaseProcess := Seq[ReleaseStep](
          checkSnapshotDependencies, // : ReleaseStep
          inquireVersions, // : ReleaseStep
          runClean, // : ReleaseStep
          runTest, // : ReleaseStep
          setReleaseVersion, // : ReleaseStep
          commitReleaseVersion, // : ReleaseStep, performs the initial git checks
          tagRelease, // : ReleaseStep
          //publishArtifacts,                       // : ReleaseStep, checks whether `publishTo` is properly set up
          setNextVersion, // : ReleaseStep
          commitNextVersion, // : ReleaseStep
          pushChanges // : ReleaseStep, also checks that an upstream branch is properly configured
        )
      )
    }
    , LibSettings -> new ProjectSettings {
      override val settings: Seq[sbt.Setting[_]] = Seq(
        Seq(
          libraryDependencies ++= R.essentials
          , libraryDependencies ++= T.essentials
        )
      ).flatten
    }
  )
}
// --------------------------------------------

val inRoot = In(".")
val inDiStage = In("distage")
  .withModuleSettings(LibSettings)
val inLogStage = In("logstage")
  .withModuleSettings(LibSettings)
val inFundamentals = In("fundamentals")
  .withModuleSettings(LibSettings)

// --------------------------------------------

lazy val fundamentalsCollections = inFundamentals.as.module
lazy val fundamentalsPlatform = inFundamentals.as.module
lazy val fundamentalsFunctional = inFundamentals.as.module

lazy val fundamentals: Seq[ProjectReferenceEx] = Seq(
  fundamentalsCollections
  , fundamentalsPlatform
  , fundamentalsFunctional
)
// --------------------------------------------
val globalDefs = setup(baseSettings)
  .withSharedLibs(fundamentals :_*)
// --------------------------------------------

lazy val fundamentalsReflection = inFundamentals.as.module
  .settings(
    libraryDependencies ++= Seq(
      R.scala_reflect
    )
  )

lazy val distageModel = inDiStage.as.module
    .depends(fundamentalsReflection)

lazy val distageMacro = inDiStage.as.module
  .depends(distageModel)
  .settings(
    libraryDependencies ++= Seq(R.scala_reflect)
  )

lazy val distageCore = inDiStage.as.module
  .depends(distageMacro, fundamentalsFunctional)
  .settings(
    libraryDependencies ++= Seq(
      R.scala_reflect
      , R.cglib_nodep
    )
  )

lazy val logstageModel = inLogStage.as.module

lazy val logstageMacro = inLogStage.as.module
  .depends(logstageModel)
  .settings(
    libraryDependencies ++= Seq(
      R.scala_reflect
    )
  )

lazy val logstageApi = inLogStage.as.module
  .depends(logstageMacro)

lazy val logstageDi = inLogStage.as.module
  .depends(logstageApi, distageModel)

lazy val logstageJsonJson4s = inLogStage.as.module
  .depends(logstageApi)
    .settings(libraryDependencies ++= Seq(R.json4s_native))

lazy val logstageSinkFile = inLogStage.as.module
  .depends(logstageApi)

lazy val logstageSinkConsole = inLogStage.as.module
  .depends(logstageApi)

lazy val logstageSinkSlf4j = inLogStage.as.module
  .depends(logstageApi)
  .settings(libraryDependencies ++= Seq(R.slf4j_api, T.slf4j_simple))

lazy val logstageAdapterSlf4j = inLogStage.as.module
  .depends(logstageApi)
  .settings(libraryDependencies += R.slf4j_api)


lazy val logstageRouting = inLogStage.as.module
  .depends(
    logstageApi
    , logstageSinkConsole.testOnlyRef
    , logstageSinkSlf4j.testOnlyRef
    , logstageJsonJson4s.testOnlyRef
  )

lazy val sbtIzumi = inRoot.as
  .module
  .enablePlugins(ScriptedPlugin)
  .settings(
    target ~= { t => t.toPath.resolve("primary").toFile }
    , scriptedLaunchOpts := {
      scriptedLaunchOpts.value ++
        Seq("-Xmx1024M", "-Dplugin.version=" + version.value)
    }
    , scriptedBufferLog := false
    , crossScalaVersions := Seq(
      scala_212
    )
  )



lazy val logstage: Seq[ProjectReference] = Seq(
  logstageDi
  , logstageRouting
  , logstageSinkConsole
  , logstageSinkFile
  , logstageSinkSlf4j
  , logstageAdapterSlf4j
)
lazy val distage: Seq[ProjectReference] = Seq(
  distageCore
)
lazy val izsbt: Seq[ProjectReference] = Seq(
  sbtIzumi
)

lazy val allProjects = distage ++ logstage ++ izsbt

lazy val root = inRoot.as
  .root
  .transitiveAggregate(allProjects: _*)



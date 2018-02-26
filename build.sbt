import com.dancingrobot84.sbtidea.tasks.{UpdateIdea => updateIdeaTask}

enablePlugins(SbtIdeaPlugin)

name := "intellij-hocon"

version := "1.0"

scalaVersion := "2.12.4"

ideaBuild in ThisBuild := "173.4301.1"

ideaDownloadDirectory in ThisBuild := Path.userHome / ".ScalaPluginIC" / "sdk"

onLoad in Global := ((s: State) => { "updateIdea" :: s}) compose (onLoad in Global).value

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test"

resourceDirectory in Compile := baseDirectory.value / "resources"

ideaInternalPlugins := Seq("properties")

updateIdea := {
  updateIdeaTask(ideaBaseDirectory.value, IdeaEdition.Community, ideaBuild.in(ThisBuild).value,
    downloadSources = true, Seq.empty, streams.value)
}

fork in Test := true

parallelExecution in Test := false

logBuffered in Test := false

javaOptions in Test := Seq(
  "-Xms128m",
  "-Xmx4096m",
  "-server",
  "-ea"
//  s"-Didea.system.path=${testSystemDir.value}",
//  s"-Didea.config.path=${testConfigDir.value}",
//  s"-Dsbt.ivy.home=$ivyHomeDir",
//  s"-Dplugin.path=${packagedPluginDir.value}"
  // to enable debugging of tests running in external sbt instance
  //      ,"-agentlib:jdwp=transport=dt_socket,server=y,address=5005,suspend=y"
)

envVars in Test += "NO_FS_ROOTS_ACCESS_CHECK" -> "yes"
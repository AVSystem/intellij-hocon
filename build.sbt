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
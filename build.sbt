import org.jetbrains.sbtidea.Keys._

ideaPluginName in ThisBuild := "intellij-hocon"

ideaBuild in ThisBuild := "191.5109.14"

lazy val hocon = project.in(file("."))
  .settings(
    scalaVersion := "2.12.8",
    version := "2019.1",
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "test",
    resourceDirectory in Compile := baseDirectory.value / "resources",
    ideaInternalPlugins := Seq("properties"),
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    ideaPublishSettings := PublishSettings("10481", sys.env.getOrElse("PL_USER", ""), sys.env.getOrElse("PL_PASS", ""), None),
    packageLibraryMappings := Seq.empty // allow scala-library
  )

lazy val runner = createRunnerProject(hocon, "hocon-runner")
import org.jetbrains.sbtidea.Keys._

ideaPluginName in ThisBuild := "intellij-hocon"

ideaBuild in ThisBuild := "192.5728.98"

lazy val hocon = project.in(file(".")).settings(
  scalaVersion := "2.12.8",
  version := "2019.2.2",
  scalaSource in Compile := baseDirectory.value / "src",
  scalaSource in Test := baseDirectory.value / "test",
  resourceDirectory in Compile := baseDirectory.value / "resources",
  javacOptions in Global ++= Seq("-source", "1.8", "-target", "1.8"),
  scalacOptions in Global ++= Seq("-target:jvm-1.8"),
  ideaInternalPlugins := Seq("properties"),
  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
  ideaPublishSettings := PublishSettings("10481", sys.env.getOrElse("PL_USER", ""), sys.env.getOrElse("PL_PASS", ""), None),
  packageLibraryMappings := Seq.empty // allow scala-library
)

lazy val runner = createRunnerProject(hocon, "hocon-runner")

import org.jetbrains.sbtidea.Keys._

ThisBuild / scalaVersion := "2.13.8"
ThisBuild / intellijPluginName := "intellij-hocon"
ThisBuild / intellijBuild := "221.3427.89"
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("11"))

val junitInterfaceVersion = "0.11"

lazy val hocon = project.in(file(".")).enablePlugins(SbtIdeaPlugin).settings(
  version := "2022.1.99-SNAPSHOT",
  Compile / scalaSource := baseDirectory.value / "src",
  Test / scalaSource := baseDirectory.value / "test",
  Compile / resourceDirectory := baseDirectory.value / "resources",
  Global / javacOptions ++= Seq("-source", "11", "-target", "11"),
  Global / scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xfatal-warnings",
  ),
  ideBasePackages := Seq("org.jetbrains.plugins.hocon"),
  intellijPlugins := Seq("com.intellij.properties", "com.intellij.java", "com.intellij.java-i18n").map(_.toPlugin),
  libraryDependencies ++= Seq(
    "com.novocode" % "junit-interface" % junitInterfaceVersion % Test,
  ),
  packageLibraryMappings := Seq.empty, // allow scala-library
  patchPluginXml := pluginXmlOptions { xml =>
    xml.version = version.value
  }
)

import org.jetbrains.sbtidea.Keys._

ThisBuild / intellijPluginName := "intellij-hocon"
ThisBuild / intellijBuild := "212.3724.25"

val junitInterfaceVersion = "0.11"
val silencerVersion = "1.7.5"

lazy val hocon = project.in(file(".")).enablePlugins(SbtIdeaPlugin).settings(
  scalaVersion := "2.13.6",
  version := "2021.2.99-SNAPSHOT",
  Compile / scalaSource := baseDirectory.value / "src",
  Test / scalaSource := baseDirectory.value / "test",
  Compile / resourceDirectory := baseDirectory.value / "resources",
  Global / javacOptions ++= Seq("-source", "11", "-target", "11"),
  Global / scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xfuture",
    "-Xfatal-warnings",
    "-P:silencer:checkUnused"
  ),
  ideBasePackages := Seq("org.jetbrains.plugins.hocon"),
  Compile / ideOutputDirectory := Some(file("out/production")),
  Test / ideOutputDirectory := Some(file("out/test")),
  intellijPlugins := Seq("com.intellij.properties", "com.intellij.java", "com.intellij.java-i18n").map(_.toPlugin),
  libraryDependencies ++= Seq(
    "com.novocode" % "junit-interface" % junitInterfaceVersion % Test,
    "com.github.ghik" % "silencer-lib" % silencerVersion % Provided cross CrossVersion.full,
    compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full),
  ),
  packageLibraryMappings := Seq.empty, // allow scala-library
  patchPluginXml := pluginXmlOptions { xml =>
    xml.version = version.value
  }
)

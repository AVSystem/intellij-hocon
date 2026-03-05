import org.jetbrains.sbtidea.Keys._

ThisBuild / scalaVersion := "2.13.18"
ThisBuild / intellijPluginName := "intellij-hocon"
ThisBuild / intellijBuild := "261.20869.38"
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("21"))
ThisBuild / autoRemoveOldCachedIntelliJSDK := true

val junitInterfaceVersion = "0.13.3"
val junitVersion = "4.13.2"
val commonsTextVersion = "1.15.0"
val opentest4jVersion = "1.3.0"

lazy val hocon = project
  .in(file("."))
  .enablePlugins(SbtIdeaPlugin)
  .settings(
    version := "2026.1.2-SNAPSHOT",
    Compile / scalaSource := baseDirectory.value / "src",
    Test / scalaSource := baseDirectory.value / "test",
    Compile / resourceDirectory := baseDirectory.value / "resources",
    Global / javacOptions ++= Seq("--release", "21"),
    Global / scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xsource:3",
    ),
    ideBasePackages := Seq("org.jetbrains.plugins.hocon"),
    intellijPlugins := Seq("com.intellij.java-i18n", "com.intellij.modules.json").map(_.toPlugin),
    intellijExtraRuntimePluginsInTests := Seq("org.jetbrains.kotlin").map(_.toPlugin),
    resolvers += "JetBrains Intellij Repository" at "https://www.jetbrains.com/intellij-repository/snapshots",
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-text" % commonsTextVersion,
      "com.github.sbt" % "junit-interface" % junitInterfaceVersion % Test,
      "junit" % "junit" % junitVersion % Test,
      "org.opentest4j" % "opentest4j" % opentest4jVersion % Test,
      "com.jetbrains.intellij.platform" % "test-framework" % intellijBuild.value % Test intransitive(),
      "com.jetbrains.intellij.java" % "java-test-framework" % intellijBuild.value % Test intransitive(),
      "com.jetbrains.intellij.java" % "java-test-framework-shared" % intellijBuild.value % Test intransitive(),
      "com.jetbrains.intellij.platform" % "test-framework-core" % intellijBuild.value % Test intransitive(),
      "com.jetbrains.intellij.platform" % "test-framework-common" % intellijBuild.value % Test intransitive(),
    ),
    packageLibraryMappings := Seq.empty, // allow scala-library
    patchPluginXml := pluginXmlOptions { xml =>
      xml.version = version.value
    },
  )

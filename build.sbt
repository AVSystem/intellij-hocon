import org.jetbrains.sbtidea.Keys._

ThisBuild / scalaVersion := "3.8.0-RC5"
ThisBuild / intellijPluginName := "intellij-hocon"
ThisBuild / intellijBuild := "252.26830.84"
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))
ThisBuild / autoRemoveOldCachedIntelliJSDK := true

val junitInterfaceVersion = "0.13.3"
val junitVersion = "4.13.2"
val commonsTextVersion = "1.14.0"
val opentest4jVersion = "1.3.0"

lazy val hocon = project
  .in(file("."))
  .enablePlugins(SbtIdeaPlugin)
  .settings(
    version := "2025.1.99-SNAPSHOT",
    Compile / scalaSource := baseDirectory.value / "src",
    Test / scalaSource := baseDirectory.value / "test",
    Compile / resourceDirectory := baseDirectory.value / "resources",
    Global / javacOptions ++= Seq("-source", "17", "-target", "17"),
    Global / scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-deprecation",
      "-explain",
      "-old-syntax",
      "-unchecked",
      "-language:noAutoTupling",
      "-Vprofile",
      "-Ycheck:all",
      "-Ycheck:macros",
      "-Ydebug-missing-refs",
      "-Yexplain-lowlevel",
      "-Yexplicit-nulls",
      "-Wsafe-init",
      "-Yshow-suppressed-errors",
      "-Yshow-var-bounds",
      "-Werror",
      "-Wunused:all",
      "-preview",
    ),
    ideBasePackages := Seq("org.jetbrains.plugins.hocon"),
    intellijPlugins := Seq("com.intellij.java-i18n", "com.intellij.modules.json").map(_.toPlugin),
    intellijExtraRuntimePluginsInTests := Seq("org.jetbrains.kotlin").map(_.toPlugin),
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-text" % commonsTextVersion,
      "com.github.sbt" % "junit-interface" % junitInterfaceVersion % Test,
      "junit" % "junit" % junitVersion % Test,
      "org.opentest4j" % "opentest4j" % opentest4jVersion % Test,
    ),
    packageLibraryMappings := Seq.empty, // allow scala-library
    patchPluginXml := pluginXmlOptions { xml =>
      xml.version = version.value
    },
  )

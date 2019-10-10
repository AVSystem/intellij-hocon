import org.jetbrains.sbtidea.Keys._

intellijPluginName in ThisBuild := "intellij-hocon"
intellijBuild in ThisBuild := "193.3793.14"

val junitInterfaceVersion = "0.11"
val silencerVersion = "1.4.4"

lazy val hocon = project.in(file(".")).enablePlugins(SbtIdeaPlugin).settings(
  scalaVersion := "2.12.10",
  version := "2019.3.2",
  scalaSource in Compile := baseDirectory.value / "src",
  scalaSource in Test := baseDirectory.value / "test",
  resourceDirectory in Compile := baseDirectory.value / "resources",
  javacOptions in Global ++= Seq("-source", "1.8", "-target", "1.8"),
  scalacOptions in Global ++= Seq(
    "-target:jvm-1.8",
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xfuture",
    "-Xfatal-warnings",
    "-P:silencer:checkUnused"
  ),
  ideBasePackages := Seq("org.jetbrains.plugins.hocon"),
  ideOutputDirectory in Compile := Some(file("out/production")),
  ideOutputDirectory in Test := Some(file("out/test")),
  intellijInternalPlugins := Seq("properties", "java", "java-i18n"),
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

lazy val runner = createRunnerProject(hocon, "hocon-runner")

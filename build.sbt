import org.jetbrains.sbtidea.Keys._

ideaPluginName in ThisBuild := "intellij-hocon"
ideaBuild in ThisBuild := "192.6603.28"

val junitInterfaceVersion = "0.11"
val silencerVersion = "1.4.3"

lazy val hocon = project.in(file(".")).settings(
  scalaVersion := "2.12.9",
  version := "2019.2.3-avs-beta",
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
    "-P:silencer:checkUnused",
  ),
  ideBasePackages := Seq("org.jetbrains.plugins.hocon"),
  ideOutputDirectory in Compile := Some(file("out/production")),
  ideOutputDirectory in Test := Some(file("out/test")),
  ideaInternalPlugins := Seq("properties", "java", "java-i18n"),
  libraryDependencies ++= Seq(
    "com.novocode" % "junit-interface" % junitInterfaceVersion % Test,
    "com.github.ghik" % "silencer-lib" % silencerVersion % Provided cross CrossVersion.full,
    compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full),
  ),
  ideaPublishSettings := PublishSettings("10481", sys.env.getOrElse("PL_USER", ""), sys.env.getOrElse("PL_PASS", ""), None),
  packageLibraryMappings := Seq.empty // allow scala-library
)

lazy val runner = createRunnerProject(hocon, "hocon-runner")

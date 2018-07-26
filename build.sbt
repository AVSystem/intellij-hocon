ideaPluginName in ThisBuild := "intellij-hocon"

ideaBuild in ThisBuild := "182.3684.2"

lazy val hocon = project.in(file("."))
  .settings(
    scalaVersion := "2.12.4",
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "test",
    resourceDirectory in Compile := baseDirectory.value / "resources",
    ideaInternalPlugins := Seq("properties"),
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    packageLibraryMappings := Seq.empty // allow scala-library
  )

lazy val runner = project.in(file("target") / "tools" / "runner")
  .settings(
    dumpDependencyStructure := null, // avoid cyclic dependencies on products task
    products := packagePlugin.in(hocon).value :: Nil,
    packageMethod := org.jetbrains.sbtidea.Keys.PackagingMethod.Skip(),
    unmanagedJars in Compile := ideaMainJars.value,
    unmanagedJars in Compile += file(System.getProperty("java.home")).getParentFile / "lib" / "tools.jar"
  )
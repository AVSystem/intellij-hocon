name := "intellij-hocon"

organization := "JetBrains"

scalaVersion := "2.11.2"

def readIdeaPropery(key: String): String = {
  import java.util.Properties
  val prop = new Properties()
  IO.load(prop, file("idea.properties"))
  prop.getProperty(key)
}

lazy val ideaBasePath = "SDK/ideaSDK/idea-" +  readIdeaPropery(   "ideaVersion")

unmanagedJars in Compile ++= (baseDirectory.value.getParentFile / ideaBasePath / "lib" * "*.jar").classpath
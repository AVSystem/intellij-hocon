import org.jetbrains.sbtidea.AbstractSbtIdeaPlugin

object AutoSbtIdeaPlugin extends AbstractSbtIdeaPlugin {
  override def requires = sbt.plugins.JvmPlugin
  override def trigger = allRequirements
}

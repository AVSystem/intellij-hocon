package org.jetbrains.plugins.hocon
package settings

import com.intellij.openapi.options.Configurable
import com.intellij.openapi.project.Project
import javax.swing.JComponent

class HoconProjectSettingsConfigurable(project: Project) extends Configurable {
  private var panel = new HoconProjectSettingsPanel(project)

  override def getDisplayName = "HOCON"

  override def isModified: Boolean = panel.isModified

  override def createComponent(): JComponent = panel.getMainComponent

  override def disposeUIResources(): Unit = {
    panel = null
  }

  override def apply(): Unit = panel.apply()

  override def reset(): Unit = panel.loadSettings()
}

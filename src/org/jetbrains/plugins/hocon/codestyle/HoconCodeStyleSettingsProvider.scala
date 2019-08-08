package org.jetbrains.plugins.hocon
package codestyle

import com.intellij.application.options.{CodeStyleAbstractConfigurable, TabbedLanguageCodeStylePanel}
import com.intellij.psi.codeStyle.{CodeStyleConfigurable, CodeStyleSettings, CodeStyleSettingsProvider, DisplayPriority}
import org.jetbrains.plugins.hocon.lang.HoconLanguage

class HoconCodeStyleSettingsProvider extends CodeStyleSettingsProvider {

  override def getConfigurableDisplayName = "HOCON"

  override def getPriority = DisplayPriority.COMMON_SETTINGS

  override def createConfigurable(settings: CodeStyleSettings, modelSettings: CodeStyleSettings): CodeStyleConfigurable =
    new CodeStyleAbstractConfigurable(settings, modelSettings, "HOCON") {
      override protected def createPanel(settings: CodeStyleSettings): TabbedLanguageCodeStylePanel =
        new TabbedLanguageCodeStylePanel(HoconLanguage, getCurrentSettings, settings) {}
    }

  override def createCustomSettings(settings: CodeStyleSettings) =
    new HoconCustomCodeStyleSettings(settings)
}

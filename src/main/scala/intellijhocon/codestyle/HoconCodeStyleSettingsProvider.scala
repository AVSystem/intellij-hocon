package intellijhocon.codestyle

import com.intellij.psi.codeStyle.{DisplayPriority, CodeStyleSettings, CodeStyleSettingsProvider}
import com.intellij.application.options.CodeStyleAbstractConfigurable

class HoconCodeStyleSettingsProvider extends CodeStyleSettingsProvider {

  override def getConfigurableDisplayName = "HOCON"

  override def getPriority = DisplayPriority.COMMON_SETTINGS

  def createSettingsPage(settings: CodeStyleSettings, originalSettings: CodeStyleSettings) =
    new CodeStyleAbstractConfigurable(settings, originalSettings, "HOCON") {
      override protected def createPanel(settings: CodeStyleSettings) =
        new HoconTabbedLanguageCodeStylePanel(getCurrentSettings, settings)

      def getHelpTopic = null

    }

  override def createCustomSettings(settings: CodeStyleSettings) =
    new HoconCustomCodeStyleSettings(settings)
}

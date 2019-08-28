package org.jetbrains.plugins.hocon
package lang

import com.intellij.openapi.fileTypes.LanguageFileType
import javax.swing.Icon

object HoconFileType extends LanguageFileType(HoconLanguage) {
  val DefaultExtension = "conf"

  def getIcon: Icon = HoconIcon
  def getDefaultExtension: String = DefaultExtension
  def getDescription = "HOCON"
  def getName = "HOCON"
}

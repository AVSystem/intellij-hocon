package org.jetbrains.plugins.hocon.lang

import com.intellij.icons.AllIcons
import com.intellij.openapi.fileTypes.LanguageFileType
import javax.swing.Icon

object HoconFileType extends LanguageFileType(HoconLanguage) {
  val DefaultExtension = "conf"

  def getIcon: Icon = AllIcons.FileTypes.Config
  def getDefaultExtension: String = DefaultExtension
  def getDescription = "HOCON"
  def getName = "HOCON"
}

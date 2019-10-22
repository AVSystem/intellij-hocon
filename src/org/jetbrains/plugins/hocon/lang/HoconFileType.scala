package org.jetbrains.plugins.hocon
package lang

import com.intellij.openapi.fileTypes.{FileType, LanguageFileType}
import javax.swing.Icon

final class HoconFileType extends LanguageFileType(HoconLanguage) {
  def getIcon: Icon = HoconIcon
  def getDefaultExtension: String = HoconFileType.DefaultExtension
  def getDescription = "HOCON"
  def getName = "HOCON"
}
object HoconFileType {
  val DefaultExtension = "conf"

  def isHocon(fileType: FileType): Boolean = fileType match {
    case _: HoconFileType => true
    case _ => false
  }
}

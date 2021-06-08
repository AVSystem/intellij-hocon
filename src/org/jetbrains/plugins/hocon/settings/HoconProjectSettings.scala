package org.jetbrains.plugins.hocon
package settings

import com.intellij.openapi.application.PathManager
import com.intellij.openapi.components._
import com.intellij.openapi.project.Project
import com.intellij.util.xmlb.XmlSerializerUtil

import java.io.File
import scala.annotation.nowarn
import scala.beans.BeanProperty

@State(
  name = "HoconProjectSettings",
  storages = Array(
    new Storage(StoragePathMacros.WORKSPACE_FILE),
    new Storage("hocon_settings.xml")
  )
)
@nowarn("msg=deprecated")
class HoconProjectSettings extends PersistentStateComponent[HoconProjectSettings] with ExportableComponent {
  def getState: HoconProjectSettings = this

  def loadState(state: HoconProjectSettings): Unit =
    XmlSerializerUtil.copyBean(state, this)

  def getPresentableName = "HOCON Project Settings"

  def getExportFiles: Array[File] =
    Array(PathManager.getOptionsFile("hocon_project_settings"))

  @BeanProperty var classReferencesOnUnquotedStrings = true
  @BeanProperty var classReferencesOnQuotedStrings = true
  @BeanProperty var propertyReferencesOnStrings = true
  @BeanProperty var searchInGotoSymbol = false
}

object HoconProjectSettings {
  def getInstance(project: Project): HoconProjectSettings =
    project.getService(classOf[HoconProjectSettings])
}

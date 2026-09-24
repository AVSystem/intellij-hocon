package org.jetbrains.plugins.hocon
package settings

import com.intellij.openapi.components.State.NameGetter
import com.intellij.openapi.components.*
import com.intellij.openapi.project.Project
import com.intellij.util.xmlb.XmlSerializerUtil

import scala.beans.BeanProperty

@State(
  name = "HoconProjectSettings",
  storages = Array(
    new Storage(StoragePathMacros.WORKSPACE_FILE),
    new Storage("hocon_settings.xml"),
  ),
  presentableName = classOf[HoconSettingsNameGetter],
  additionalExportDirectory = "hocon_project_settings.xml",
)
class HoconProjectSettings extends PersistentStateComponent[HoconProjectSettings] {
  def getState: HoconProjectSettings = this

  def loadState(state: HoconProjectSettings): Unit =
    XmlSerializerUtil.copyBean(state, this)

  @BeanProperty var classReferencesOnUnquotedStrings = true
  @BeanProperty var classReferencesOnQuotedStrings = true
  @BeanProperty var propertyReferencesOnStrings = true
  @BeanProperty var searchInGotoSymbol = false
  @BeanProperty var fileNavigationExtensions: String = ""
  @BeanProperty var fileNavigationSearchRoots: String = ""

  def fileNavigationExtensionsList: List[String] =
    if (fileNavigationExtensions.trim.isEmpty) Nil
    else fileNavigationExtensions.split(",").map(_.trim).filter(_.nonEmpty).toList

  def fileNavigationSearchRootsList: List[String] =
    if (fileNavigationSearchRoots.trim.isEmpty) Nil
    else fileNavigationSearchRoots.split(",").map(_.trim).filter(_.nonEmpty).toList
}

object HoconProjectSettings {
  def getInstance(project: Project): HoconProjectSettings =
    project.getService(classOf[HoconProjectSettings])
}

class HoconSettingsNameGetter extends NameGetter {
  def get(): String = "HOCON Project Settings"
}

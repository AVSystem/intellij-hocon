package org.jetbrains.plugins.hocon
package navigation

import java.io.File

import com.intellij.navigation.{ChooseByNameContributorEx, ItemPresentation, NavigationItem, PsiElementNavigationItem}
import com.intellij.openapi.application.ReadAction
import com.intellij.openapi.roots.ProjectFileIndex
import com.intellij.psi.PsiElement
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.util.Processor
import com.intellij.util.indexing.{FileBasedIndex, FindSymbolParameters, IdFilter}
import javax.swing.Icon
import org.jetbrains.plugins.hocon.indexing.HoconKeyIndex
import org.jetbrains.plugins.hocon.psi.{HKey, HObjectEntries}
import org.jetbrains.plugins.hocon.settings.HoconProjectSettings

import scala.collection.mutable

class HoconGotoSymbolContributor extends ChooseByNameContributorEx {
  private def enabled(scope: GlobalSearchScope): Boolean =
    scope.getProject.opt.exists(proj => HoconProjectSettings.getInstance(proj).searchInGotoSymbol)

  def processNames(processor: Processor[String], scope: GlobalSearchScope, filter: IdFilter): Unit =
    FileBasedIndex.getInstance.processAllKeys(HoconKeyIndex.Id, processor, scope, filter)

  def processElementsWithName(name: String, processor: Processor[NavigationItem], parameters: FindSymbolParameters): Unit =
    if (enabled(parameters.getSearchScope)) ReadAction.run { () =>
      val project = parameters.getProject
      val scope = parameters.getSearchScope
      val locationsSeen = new mutable.HashSet[(HObjectEntries, List[String])]
      HoconKeyIndex.processHKeys(name, project, scope, _.inFields.reverseIterator) { foundKey =>
        val item = for {
          entries <- foundKey.field.map(_.outermostEntries)
          keyPath <- foundKey.fullContainingPath
          if locationsSeen.add((entries, keyPath.map(_.stringValue)))
        } yield HoconGotoSymbolItem(foundKey)
        item.forall(processor.process)
      }
    }
}

case class HoconGotoSymbolItem(key: HKey) extends PsiElementNavigationItem with ItemPresentation {
  def getTargetElement: PsiElement = key
  def getName: String = key.getText
  def navigate(requestFocus: Boolean): Unit = key.navigate(requestFocus)
  def canNavigate: Boolean = key.canNavigate
  def canNavigateToSource: Boolean = key.canNavigateToSource
  def getPresentation: ItemPresentation = this
  def getPresentableText: String = key.fullPathText.orNull
  def getIcon(unused: Boolean): Icon = PropertyIcon
  def getLocationString: String =
    key.hoconFile.getVirtualFile.opt.map { vf =>
      val pfi = ProjectFileIndex.getInstance(key.getProject)
      val rootDir = pfi.getContentRootForFile(vf).opt orElse pfi.getClassRootForFile(vf).opt
      val rootPath = rootDir.fold("")(_.getPath)
      s"(in ${vf.getPath.stripPrefix(rootPath).stripPrefix(File.separator)})"
    }.orNull
}

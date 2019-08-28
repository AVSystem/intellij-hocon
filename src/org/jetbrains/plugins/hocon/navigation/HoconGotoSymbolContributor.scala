package org.jetbrains.plugins.hocon
package navigation

import java.io.File

import com.intellij.navigation.{ChooseByNameContributorEx, ItemPresentation, NavigationItem, PsiElementNavigationItem}
import com.intellij.openapi.application.ReadAction
import com.intellij.openapi.roots.ProjectFileIndex
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.{PsiElement, PsiManager}
import com.intellij.util.Processor
import com.intellij.util.indexing.FileBasedIndex.ValueProcessor
import com.intellij.util.indexing.{FileBasedIndex, FindSymbolParameters, IdFilter}
import javax.swing.Icon
import org.jetbrains.plugins.hocon.psi.{HKeyedField, HObjectEntries, HoconPsiFile}
import org.jetbrains.plugins.hocon.settings.HoconProjectSettings

import scala.collection.mutable

class HoconGotoSymbolContributor extends ChooseByNameContributorEx {
  private def enabled(scope: GlobalSearchScope): Boolean =
    scope.getProject.opt.exists(proj => HoconProjectSettings.getInstance(proj).searchInGotoSymbol)

  def processNames(processor: Processor[String], scope: GlobalSearchScope, filter: IdFilter): Unit =
    FileBasedIndex.getInstance.processAllKeys(HoconKeyIndex.Id, processor, scope, filter)

  def processElementsWithName(name: String, processor: Processor[NavigationItem], parameters: FindSymbolParameters): Unit =
    if (enabled(parameters.getSearchScope)) {
      val psim = PsiManager.getInstance(parameters.getProject)
      val pfi = ProjectFileIndex.getInstance(parameters.getProject)
      val locationsSeen = new mutable.HashSet[(HObjectEntries, List[String])]
      val indexProcessor: ValueProcessor[HKeyOccurrences] = (file, occurrences) => {
        for {
          f <- file.opt if !pfi.isInLibrarySource(f)
          hoconFile <- psim.findFile(f).opt.collectOnly[HoconPsiFile]
          range <- occurrences.inFields.reverseIterator
          field <- hoconFile.findElementAt(range.getStartOffset).parentOfType[HKeyedField]
          (entries, keyPath) <- field.fullContainingPath
        } {
          // process only the last entry for its full path (within containing entries)
          if (locationsSeen.add((entries, keyPath.map(_.stringValue)))) {
            processor.process(HoconGotoSymbolItem(field))
          }
        }
        true
      }
      ReadAction.run { () =>
        FileBasedIndex.getInstance.processValues(
          HoconKeyIndex.Id, name, null, indexProcessor, parameters.getSearchScope, parameters.getIdFilter)
      }
    }
}

case class HoconGotoSymbolItem(field: HKeyedField) extends PsiElementNavigationItem with ItemPresentation {
  def getTargetElement: PsiElement = field
  def getName: String = field.getName
  def navigate(requestFocus: Boolean): Unit = field.navigate(requestFocus)
  def canNavigate: Boolean = field.canNavigate
  def canNavigateToSource: Boolean = field.canNavigateToSource
  def getPresentation: ItemPresentation = this
  def getPresentableText: String = field.getQualifiedName
  def getIcon(unused: Boolean): Icon = PropertyIcon
  def getLocationString: String =
    field.hoconFile.getVirtualFile.opt.map { vf =>
      val rootPath = ProjectFileIndex.getInstance(field.getProject).getContentRootForFile(vf).opt.fold("")(_.getPath)
      s"(in ${vf.getPath.stripPrefix(rootPath).stripPrefix(File.separator)})"
    }.orNull
}

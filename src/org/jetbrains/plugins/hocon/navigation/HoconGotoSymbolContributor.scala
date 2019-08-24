package org.jetbrains.plugins.hocon
package navigation

import com.intellij.navigation.{ChooseByNameContributorEx, NavigationItem}
import com.intellij.openapi.application.ReadAction
import com.intellij.openapi.roots.ProjectFileIndex
import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiManager
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.util.Processor
import com.intellij.util.indexing.FileBasedIndex.ValueProcessor
import com.intellij.util.indexing.{FileBasedIndex, FindSymbolParameters, IdFilter}
import org.jetbrains.plugins.hocon.psi.{HKey, HoconPsiFile}

class HoconGotoSymbolContributor extends ChooseByNameContributorEx {
  def processNames(processor: Processor[String], scope: GlobalSearchScope, filter: IdFilter): Unit =
    FileBasedIndex.getInstance.processAllKeys(HoconKeyIndex.Id, processor, scope, filter)

  def processElementsWithName(name: String, processor: Processor[NavigationItem], parameters: FindSymbolParameters): Unit = {
    val psim = PsiManager.getInstance(parameters.getProject)
    val pfi = ProjectFileIndex.getInstance(parameters.getProject)
    val indexProcessor: ValueProcessor[List[TextRange]] = (file, ranges) => {
      for {
        f <- file.opt if !pfi.isInLibrarySource(f)
        hoconFile <- psim.findFile(f).opt.collectOnly[HoconPsiFile]
        range <- ranges
        foundKey <- hoconFile.findElementAt(range.getStartOffset).parentOfType[HKey] if foundKey.inField
      } {
        processor.process(foundKey)
      }
      true
    }
    ReadAction.run { () =>
      FileBasedIndex.getInstance.processValues(
        HoconKeyIndex.Id, name, null, indexProcessor, parameters.getSearchScope, parameters.getIdFilter)
    }
  }
}

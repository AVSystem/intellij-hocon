package org.jetbrains.plugins.hocon
package navigation

import com.intellij.find.findUsages.{CustomUsageSearcher, FindUsagesOptions}
import com.intellij.lang.HelpID
import com.intellij.lang.cacheBuilder.{DefaultWordsScanner, WordsScanner}
import com.intellij.lang.findUsages.FindUsagesProvider
import com.intellij.openapi.application.ReadAction
import com.intellij.openapi.roots.ProjectFileIndex
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.tree.TokenSet
import com.intellij.psi.{PsiElement, PsiManager}
import com.intellij.usageView.UsageInfo
import com.intellij.usages.{ReadWriteAccessUsageInfo2UsageAdapter, Usage}
import com.intellij.util.Processor
import com.intellij.util.indexing.FileBasedIndex
import com.intellij.util.indexing.FileBasedIndex.ValueProcessor
import org.jetbrains.plugins.hocon.lexer.{HoconLexer, HoconTokenSets, HoconTokenType}
import org.jetbrains.plugins.hocon.psi.{HKey, HoconPsiFile}

class HoconFindUsagesProvider extends FindUsagesProvider {
  def getWordsScanner: WordsScanner = new DefaultWordsScanner(new HoconLexer,
    TokenSet.EMPTY, HoconTokenSets.Comment, HoconTokenSets.StringLiteral | HoconTokenType.UnquotedChars)

  def canFindUsagesFor(psiElement: PsiElement): Boolean = psiElement match {
    case hkey: HKey => hkey.isValidKey
    case _ => false
  }

  def getHelpId(psiElement: PsiElement): String = HelpID.FIND_OTHER_USAGES

  def getType(element: PsiElement): String = "property"

  def getDescriptiveName(element: PsiElement): String =
    getNodeText(element, useFullName = true)

  def getNodeText(element: PsiElement, useFullName: Boolean): String = element match {
    case key: HKey =>
      if (useFullName) key.getQualifiedName else key.getName
    case _ => null
  }
}

class HoconUsageSearcher extends CustomUsageSearcher {
  def processElementUsages(
    element: PsiElement, processor: Processor[Usage], options: FindUsagesOptions
  ): Unit = element match {
    case hkey: HKey => ReadAction.run { () =>
      val manager = PsiManager.getInstance(hkey.getProject)
      val pfi = ProjectFileIndex.getInstance(hkey.getProject)
      for {
        globalSearchScope <- options.searchScope.opt.collectOnly[GlobalSearchScope]
        key <- element.opt.collectOnly[HKey]
        (entries, keyPath) <- key.fullContainingPath if entries.isToplevel
      } {
        val pathStr = keyPath.map(_.stringValue)
        val indexProcessor: ValueProcessor[HKeyOccurrences] = (file, occurrences) => {
          for {
            f <- file.opt.filterNot(pfi.isInLibrarySource)
            hoconFile <- manager.findFile(f).opt.collectOnly[HoconPsiFile]
            range <- occurrences.all
            foundKey <- hoconFile.findElementAt(range.getStartOffset).parentOfType[HKey]
          } {
            val usageInfo = new UsageInfo(foundKey, 0, foundKey.getTextLength, true)
            processor.process(new ReadWriteAccessUsageInfo2UsageAdapter(
              usageInfo, foundKey.inSubstitution, foundKey.inField))
          }
          true
        }
        FileBasedIndex.getInstance.processValues(HoconPathIndex.Id, pathStr, null, indexProcessor, globalSearchScope)
      }
    }
    case _ =>
  }
}

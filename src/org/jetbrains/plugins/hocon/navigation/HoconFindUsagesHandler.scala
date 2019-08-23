package org.jetbrains.plugins.hocon
package navigation

import com.intellij.find.findUsages.{FindUsagesHandler, FindUsagesHandlerFactory, FindUsagesOptions}
import com.intellij.lang.HelpID
import com.intellij.lang.cacheBuilder.{DefaultWordsScanner, WordsScanner}
import com.intellij.lang.findUsages.FindUsagesProvider
import com.intellij.openapi.application.ReadAction
import com.intellij.openapi.util.TextRange
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.tree.TokenSet
import com.intellij.psi.{PsiElement, PsiManager}
import com.intellij.usageView.UsageInfo
import com.intellij.util.Processor
import com.intellij.util.indexing.FileBasedIndex
import com.intellij.util.indexing.FileBasedIndex.ValueProcessor
import org.jetbrains.plugins.hocon.lexer.{HoconLexer, HoconTokenSets, HoconTokenType}
import org.jetbrains.plugins.hocon.psi.{HKey, HoconPsiFile}

class HoconFindUsagesHandlerFactory extends FindUsagesHandlerFactory {
  def canFindUsages(element: PsiElement): Boolean =
    element.isInstanceOf[HKey]

  def createFindUsagesHandler(element: PsiElement, forHighlightUsages: Boolean): FindUsagesHandler =
    element match {
      case hkey: HKey => new HoconFindUsagesHandler(hkey)
      case _ => null
    }
}

class HoconFindUsagesHandler(hkey: HKey) extends FindUsagesHandler(hkey) {
  private val manager = PsiManager.getInstance(hkey.getProject)

  override def processElementUsages(element: PsiElement, processor: Processor[UsageInfo], options: FindUsagesOptions): Boolean = {
    val superResult = super.processElementUsages(element, processor, options)
    ReadAction.run { () =>
      for {
        globalSearchScope <- options.searchScope.opt.collectOnly[GlobalSearchScope]
        key <- element.opt.collectOnly[HKey]
        (entries, keyPath) <- key.fullValidContainingPath if entries.isToplevel
      } {
        val pathStr = keyPath.map(_.stringValue)
        val indexProcessor: ValueProcessor[List[TextRange]] = (file, ranges) => {
          for {
            hoconFile <- manager.findFile(file).opt.collectOnly[HoconPsiFile]
            range <- ranges
            foundKey <- hoconFile.findElementAt(range.getStartOffset).parentOfType[HKey]
          } {
            processor.process(new UsageInfo(foundKey, 0, foundKey.getTextLength, true))
          }
          true
        }
        FileBasedIndex.getInstance.processValues(HoconPathIndex.Id, pathStr, null, indexProcessor, globalSearchScope)
      }
    }
    superResult
  }
}

class HoconFindUsagesProvider extends FindUsagesProvider {
  def getWordsScanner: WordsScanner = new DefaultWordsScanner(new HoconLexer,
    TokenSet.EMPTY, HoconTokenSets.Comment, HoconTokenSets.StringLiteral | HoconTokenType.UnquotedChars)

  def canFindUsagesFor(psiElement: PsiElement): Boolean = psiElement match {
    case hkey: HKey => hkey.isValidKey
    case _ => false
  }

  def getHelpId(psiElement: PsiElement): String = HelpID.FIND_OTHER_USAGES
  def getType(element: PsiElement): String = "property"
  def getDescriptiveName(element: PsiElement): String = element.getText
  def getNodeText(element: PsiElement, useFullName: Boolean): String = element.getText
}

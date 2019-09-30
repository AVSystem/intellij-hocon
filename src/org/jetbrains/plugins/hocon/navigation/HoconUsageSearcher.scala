package org.jetbrains.plugins.hocon
package navigation

import com.intellij.find.findUsages.{CustomUsageSearcher, FindUsagesOptions}
import com.intellij.lang.HelpID
import com.intellij.lang.cacheBuilder.{DefaultWordsScanner, WordsScanner}
import com.intellij.lang.findUsages.FindUsagesProvider
import com.intellij.openapi.application.ReadAction
import com.intellij.psi.PsiElement
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.tree.TokenSet
import com.intellij.usageView.UsageInfo
import com.intellij.usages.{ReadWriteAccessUsageInfo2UsageAdapter, Usage}
import com.intellij.util.Processor
import org.jetbrains.plugins.hocon.indexing.HoconPathIndex
import org.jetbrains.plugins.hocon.lexer.{HoconLexer, HoconTokenSets, HoconTokenType}
import org.jetbrains.plugins.hocon.psi.{HKey, HKeyedField, HObjectEntries, HPath}

class HoconFindUsagesProvider extends FindUsagesProvider {
  override def getWordsScanner: WordsScanner = new DefaultWordsScanner(new HoconLexer,
    TokenSet.EMPTY, HoconTokenSets.Comment, HoconTokenSets.StringLiteral | HoconTokenType.UnquotedChars)

  def canFindUsagesFor(psiElement: PsiElement): Boolean = psiElement match {
    case hkey: HKey => hkey.isValidKey
    case _ => false
  }

  def getHelpId(psiElement: PsiElement): String = HelpID.FIND_OTHER_USAGES

  def getType(element: PsiElement): String = "config property"

  def getDescriptiveName(element: PsiElement): String =
    getNodeText(element, useFullName = true)

  def getNodeText(element: PsiElement, useFullName: Boolean): String = element match {
    case key: HKey => key.fullPathText.orNull
    case _ => null
  }
}


object HoconUsageSearcher {
  private def localUsageEntries(hkey: HKey): Option[HObjectEntries] = hkey.parent match {
    case _: HPath => hkey.hoconFile.toplevelEntries
    case kf: HKeyedField => Some(kf.outermostEntries)
  }

  def localUsagesOf(hkey: HKey): Iterator[HKey] = for {
    strPath <- hkey.fullStringPath.iterator
    entriesOpt = localUsageEntries(hkey) // None is only for substitutions inside files with toplevel arrays
    occkey <- hkey.hoconFile.depthFirst.collectOnly[HKey]
    occStrPath <- occkey.fullStringPath.iterator
    if localUsageEntries(occkey) == entriesOpt && occStrPath == strPath
  } yield occkey

}
class HoconUsageSearcher extends CustomUsageSearcher {
  def processElementUsages(
    element: PsiElement, processor: Processor[Usage], options: FindUsagesOptions
  ): Unit = element match {
    case hkey: HKey => ReadAction.run { () =>
      for {
        globalSearchScope <- options.searchScope.opt.collectOnly[GlobalSearchScope]
        keyPath <- hkey.fullContainingPath
        referentiable = !hkey.inFieldInArray
      } {
        val strPath = keyPath.map(_.stringValue)

        def onKeyFound(foundKey: HKey): Boolean = {
          val usageInfo = new UsageInfo(foundKey, 0, foundKey.getTextLength, true)
          processor.process(new ReadWriteAccessUsageInfo2UsageAdapter(
            usageInfo, foundKey.inSubstitution, foundKey.inField))
        }

        if (referentiable)
          HoconPathIndex.processHKeys(strPath, hkey.getProject, globalSearchScope)(onKeyFound)
        else
          HoconUsageSearcher.localUsagesOf(hkey).foreach(onKeyFound)
      }
    }
    case _ =>
  }
}

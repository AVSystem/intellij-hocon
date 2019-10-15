package org.jetbrains.plugins.hocon
package navigation

import java.util.Collections

import com.intellij.find.findUsages.{FindUsagesHandler, FindUsagesHandlerFactory, FindUsagesOptions}
import com.intellij.lang.HelpID
import com.intellij.lang.cacheBuilder.{DefaultWordsScanner, WordsScanner}
import com.intellij.lang.findUsages.FindUsagesProvider
import com.intellij.openapi.application.ReadAction
import com.intellij.psi.search.{GlobalSearchScope, SearchScope}
import com.intellij.psi.tree.TokenSet
import com.intellij.psi.{PsiElement, PsiReference}
import com.intellij.usageView.UsageInfo
import com.intellij.util.Processor
import org.jetbrains.plugins.hocon.indexing.HoconPathIndex
import org.jetbrains.plugins.hocon.lexer.{HoconLexer, HoconTokenSets, HoconTokenType}
import org.jetbrains.plugins.hocon.psi.{HKey, HKeyedField, HObjectEntries, HPath}

class HoconWordsScanner extends DefaultWordsScanner(
  new HoconLexer,
  HoconTokenSets.StringLiteral | HoconTokenType.UnquotedChars,
  HoconTokenSets.Comment,
  TokenSet.EMPTY // strings needs to be recognized as identifiers for find usages to work
) {
  setMayHaveFileRefsInLiterals(true)
}

class HoconFindUsagesProvider extends FindUsagesProvider {
  override def getWordsScanner: WordsScanner = new HoconWordsScanner

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

class HoconFindUsagesHandlerFactory extends FindUsagesHandlerFactory {
  def canFindUsages(element: PsiElement): Boolean = element match {
    case hkey: HKey => hkey.isValidKey
    case _ => false
  }

  def createFindUsagesHandler(element: PsiElement, forHighlightUsages: Boolean): FindUsagesHandler =
    new HoconFindUsagesHandler(element)
}

class HoconFindUsagesHandler(element: PsiElement) extends FindUsagesHandler(element) {
  override def processElementUsages(
    element: PsiElement, processor: Processor[UsageInfo], options: FindUsagesOptions
  ): Boolean = element match {
    case hkey: HKey => ReadAction.compute { () =>
      val resOpt = for {
        globalSearchScope <- options.searchScope.opt.collectOnly[GlobalSearchScope]
        keyPath <- hkey.fullContainingPath
        referentiable = !hkey.inFieldInArray
      } yield {
        val strPath = keyPath.map(_.stringValue)

        def onKeyFound(foundKey: HKey): Boolean =
          processor.process(new UsageInfo(foundKey, 0, foundKey.getTextLength, false))

        if (referentiable)
          HoconPathIndex.processHKeys(strPath, hkey.getProject, globalSearchScope)(onKeyFound)
        else
          HoconFindUsagesHandler.localUsagesOf(hkey).forall(onKeyFound)
      }
      resOpt.getOrElse(true)
    }
    case _ =>
      true
  }

  override def findReferencesToHighlight(target: PsiElement, searchScope: SearchScope): JCollection[PsiReference] =
    target match {
      case hkey: HKey => HoconFindUsagesHandler.localUsagesOf(hkey).map(_.getReference).toJList
      case _ => Collections.emptyList[PsiReference]
    }
}
object HoconFindUsagesHandler {
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

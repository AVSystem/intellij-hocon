package org.jetbrains.plugins.hocon
package navigation

import com.intellij.find.findUsages.{FindUsagesHandler, FindUsagesHandlerFactory, FindUsagesOptions}
import com.intellij.lang.HelpID
import com.intellij.lang.cacheBuilder.{DefaultWordsScanner, WordsScanner}
import com.intellij.lang.findUsages.FindUsagesProvider
import com.intellij.openapi.application.ReadAction
import com.intellij.openapi.roots.ProjectFileIndex
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiElement
import com.intellij.psi.search._
import com.intellij.psi.tree.TokenSet
import com.intellij.usageView.UsageInfo
import com.intellij.util.Processor
import org.jetbrains.plugins.hocon.indexing.HoconPathIndex
import org.jetbrains.plugins.hocon.lang.HoconFileType
import org.jetbrains.plugins.hocon.lexer.{HoconLexer, HoconTokenSets, HoconTokenType}
import org.jetbrains.plugins.hocon.psi._

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
    element: PsiElement, processor: Processor[_ >: UsageInfo], options: FindUsagesOptions
  ): Boolean = {
    val res = element match {
      case hkey: HKey if options.isUsages => ReadAction.compute { () =>
        hkey.fullContainingPath.forall { keyPath =>
          val strPath = keyPath.map(_.stringValue)

          def onKeyFound(foundKey: HKey): Boolean =
            processor.process(new UsageInfo(foundKey, 0, foundKey.getTextLength, false))

          options.searchScope match {
            case gss: GlobalSearchScope if !hkey.inFieldInArray =>
              HoconPathIndex.processHKeys(strPath, hkey.getProject, gss)(onKeyFound)
            case _ =>
              HoconFindUsagesHandler.localUsagesOf(hkey).forall(onKeyFound)
          }
        }
      }
      case _ => true
    }
    res && super.processElementUsages(element, processor, options)
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

class HoconUseScopeAdjuster extends UseScopeEnlarger with ScopeOptimizer {
  def getAdditionalUseScope(element: PsiElement): SearchScope = element match {
    // HOCON properties represent paths which can be used anywhere in the project
    case fk: HFieldKey => ProjectScope.getAllScope(fk.getProject)
    case _ => null
  }

  override def getRestrictedUseScope(element: PsiElement): SearchScope = element match {
    case hk: HFieldKey =>
      val project = hk.getProject
      val pfi = ProjectFileIndex.getInstance(project)
      new EverythingGlobalScope(project) {
        // HOCON files in library sources are just duplicates of the same HOCON files in regular library jars
        override def contains(file: VirtualFile): Boolean =
          !(HoconFileType.isHocon(file.getFileType) && pfi.isInLibrarySource(file))
      }
    case _ =>
      super.getRestrictedUseScope(element)
  }
}

package org.jetbrains.plugins.hocon
package navigation

import com.intellij.codeInsight.CodeInsightActionHandler
import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler
import com.intellij.ide.util.PsiNavigationSupport
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.hocon.psi._

import scala.annotation.tailrec

class HoconGotoDeclarationHandler extends GotoDeclarationHandler {
  def getGotoDeclarationTargets(sourceElement: PsiElement, offset: Int, editor: Editor): Array[PsiElement] =
    sourceElement.parentOfType[HPath].fold(PsiElement.EMPTY_ARRAY) { path =>
      val subst = path.substitution
      val file = path.getContainingFile
      val resCtx = ToplevelCtx(file, file.toplevelEntries, ToplevelCtx.referenceFilesFor(file))
      val resField = subst.resolve(reverse = true, resCtx).nextOption

      @tailrec def gotoPrefix(rfOpt: Option[ResolvedField], subpath: HPath): Option[ResolvedField] =
        if (subpath eq path) rfOpt
        else (rfOpt, subpath.prefix) match {
          case (Some(rf), Some(ppath)) => gotoPrefix(rf.prefixField, ppath)
          case _ => None
        }

      subst.path.flatMap(fullPath => gotoPrefix(resField, fullPath))
        .map(_.field).toArray[PsiElement]
    }
}

class HoconGotoSuperHandler extends CodeInsightActionHandler {
  private def toplevelCtx(field: HKeyedField): ToplevelCtx = {
    val file = field.getContainingFile
    val entries = field.parent match {
      case of: HObjectField => of.parent
      case _ => file.toplevelEntries
    }
    ToplevelCtx(file, entries, ToplevelCtx.referenceFilesFor(file))
  }

  private def makeContextFor(field: HKeyedField): Option[ResolvedField] =
    field.validKeyString.map { key =>
      val parentCtx = field.prefixingField.flatMap(makeContextFor).getOrElse(toplevelCtx(field))
      ResolvedField(key, field, parentCtx)
    }

  def invoke(project: Project, editor: Editor, file: PsiFile): Unit = for {
    elemAtOffset <- file.findElementAt(editor.getCaretModel.getOffset).opt
    fieldAtOffset <- elemAtOffset.parentOfType[HKeyedField]
    resField <- makeContextFor(fieldAtOffset)
    prevOccurrence <- resField.nextOccurrence(reverse = true).map(_.field)
    containingFile <- prevOccurrence.getContainingFile.opt.flatMap(_.getVirtualFile.opt)
  } {
    val desc = PsiNavigationSupport.getInstance.createNavigatable(
      project, containingFile, prevOccurrence.getTextOffset)
    desc.navigate(true)
  }
}

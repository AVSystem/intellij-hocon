package org.jetbrains.plugins.hocon
package navigation

import com.intellij.codeInsight.CodeInsightActionHandler
import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler
import com.intellij.ide.util.PsiNavigationSupport
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.hocon.psi.{HKeyedField, HPath, ResolvedField, ToplevelCtx}

import scala.annotation.tailrec

class HoconGotoDeclarationHandler extends GotoDeclarationHandler {
  def getGotoDeclarationTargets(sourceElement: PsiElement, offset: Int, editor: Editor): Array[PsiElement] = {
    sourceElement.parentOfType[HPath].fold(PsiElement.EMPTY_ARRAY) { path =>
      val subst = path.substitution
      val resCtx = ToplevelCtx(path.getContainingFile)
      val fullPathResolved = for {
        fullPath <- subst.path
        resField <- subst.resolve(reverse = true, resCtx).nextOption
      } yield (fullPath, resField)

      // when resolving a prefix in a substitution (e.g. the path `a.b` in substitution `${a.b.c}`) then
      // actually resolve the full path (`a.b.c`) and simply navigate up
      @tailrec def gotoPrefix(res: Option[(HPath, ResolvedField)]): Option[HKeyedField] = res match {
        case Some((`path`, field)) => Some(field.field)
        case Some((subpath, field)) =>
          gotoPrefix(for (pref <- subpath.prefix; prefField <- field.previousField) yield (pref, prefField))
        case _ => None
      }
      gotoPrefix(fullPathResolved).toArray[PsiElement]
    }
  }
}

class HoconGotoSuperHandler extends CodeInsightActionHandler {
  def invoke(project: Project, editor: Editor, file: PsiFile): Unit = for {
    elemAtOffset <- file.findElementAt(editor.getCaretModel.getOffset).opt
    fieldAtOffset <- elemAtOffset.parentOfType[HKeyedField]
    resCtx = ToplevelCtx(fieldAtOffset.getContainingFile)
    prevOccurrence <- fieldAtOffset.moreOccurrences(reverse = true, resCtx).nextOption.map(_.field)
    containingFile <- prevOccurrence.getContainingFile.opt.flatMap(_.getVirtualFile.opt)
  } {
    val desc = PsiNavigationSupport.getInstance.createNavigatable(
      project, containingFile, prevOccurrence.getTextOffset)
    desc.navigate(true)
  }
}

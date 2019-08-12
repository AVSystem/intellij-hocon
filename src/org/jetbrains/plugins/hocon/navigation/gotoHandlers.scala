package org.jetbrains.plugins.hocon
package navigation

import com.intellij.codeInsight.CodeInsightActionHandler
import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler
import com.intellij.ide.util.PsiNavigationSupport
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.hocon.psi._

class HoconGotoDeclarationHandler extends GotoDeclarationHandler {
  def getGotoDeclarationTargets(sourceElement: PsiElement, offset: Int, editor: Editor): Array[PsiElement] =
    sourceElement.parentOfType[HPath].fold(PsiElement.EMPTY_ARRAY)(_.resolve().map(_.field).toArray[PsiElement])
}

class HoconGotoSuperHandler extends CodeInsightActionHandler {
  def invoke(project: Project, editor: Editor, file: PsiFile): Unit = for {
    elemAtOffset <- file.findElementAt(editor.getCaretModel.getOffset).opt
    fieldAtOffset <- elemAtOffset.parentOfType[HKeyedField]
    resField <- fieldAtOffset.makeContext
    prevOccurrence <- resField.nextOccurrence(reverse = true).map(_.field)
    containingFile <- prevOccurrence.getContainingFile.opt.flatMap(_.getVirtualFile.opt)
  } {
    val desc = PsiNavigationSupport.getInstance.createNavigatable(
      project, containingFile, prevOccurrence.getTextOffset)
    desc.navigate(true)
  }
}

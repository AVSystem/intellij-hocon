package org.jetbrains.plugins.hocon
package navigation

import com.intellij.codeInsight.CodeInsightActionHandler
import com.intellij.codeInsight.actions.BaseCodeInsightAction
import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler
import com.intellij.ide.util.PsiNavigationSupport
import com.intellij.openapi.actionSystem.{ActionPromoter, AnAction, DataContext}
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.hocon.lang.HoconLanguage
import org.jetbrains.plugins.hocon.psi._
import org.jetbrains.plugins.hocon.semantics.ResOpts

class HoconGotoDeclarationHandler extends GotoDeclarationHandler {
  def getGotoDeclarationTargets(sourceElement: PsiElement, offset: Int, editor: Editor): Array[PsiElement] =
    sourceElement.parentOfType[HPath].flatMap(_.resolveBest()).map(_.field).toArray[PsiElement]
}

abstract class HoconGotoPrevNextAction(reverse: Boolean) extends BaseCodeInsightAction with CodeInsightActionHandler {
  override def isValidForFile(project: Project, editor: Editor, file: PsiFile): Boolean =
    file.getLanguage == HoconLanguage

  def getHandler: CodeInsightActionHandler = this

  override def startInWriteAction: Boolean = false

  def invoke(project: Project, editor: Editor, file: PsiFile): Unit = for {
    elemAtOffset <- file.findElementAt(editor.getCaretModel.getOffset).opt
    fieldAtOffset <- elemAtOffset.parentOfType[HKeyedField]
    resField <- fieldAtOffset.makeContext
    nextOccurrence <- resField.nextOccurrence(ResOpts(reverse)).map(_.field)
    containingFile <- nextOccurrence.getContainingFile.opt.flatMap(_.getVirtualFile.opt)
  } {
    val desc = PsiNavigationSupport.getInstance.createNavigatable(
      project, containingFile, nextOccurrence.getTextOffset)
    desc.navigate(true)
  }
}

class HoconGotoPrevAction extends HoconGotoPrevNextAction(reverse = true)
class HoconGotoNextAction extends HoconGotoPrevNextAction(reverse = false)

final class HoconGotoPrevNextPromoter extends ActionPromoter {
  // prioritize HoconGoto{Prev,Next}Action over GotoImplementationAction & GotoSuperAction in HOCON files
  // ideally these two actions should be completely disabled in HOCON files but I don't know how
  def promote(actions: JList[AnAction], context: DataContext): JList[AnAction] =
    actions.iterator.asScala.collectOnly[HoconGotoPrevNextAction].toJList
}

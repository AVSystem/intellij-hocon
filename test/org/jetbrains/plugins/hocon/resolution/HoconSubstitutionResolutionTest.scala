package org.jetbrains.plugins.hocon
package resolution

import com.intellij.openapi.actionSystem.IdeActions
import com.intellij.openapi.editor.Editor
import com.intellij.psi.PsiFile

class HoconSubstitutionResolutionTest extends HoconActionTest(IdeActions.ACTION_GOTO_DECLARATION, "substitution") {
  protected def extractResult(file: PsiFile, editor: Editor): String = {
    val lp = editor.getCaretModel.getLogicalPosition
    s"${lp.line + 1}:${lp.column + 1}"
  }
}
object HoconSubstitutionResolutionTest extends TestSuiteCompanion[HoconSubstitutionResolutionTest]
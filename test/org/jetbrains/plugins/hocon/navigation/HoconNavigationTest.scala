package org.jetbrains.plugins.hocon
package navigation

import com.intellij.openapi.editor.Editor
import com.intellij.psi.PsiFile
import org.junit.runner.RunWith
import org.junit.runners.AllTests

abstract class HoconNavigationTest(actionId: String, subPath: String)
  extends HoconActionTest(actionId, subPath) {
  protected def extractResult(file: PsiFile, editor: Editor): String = {
    val lp = editor.getCaretModel.getLogicalPosition
    s"${lp.line + 1}:${lp.column + 1}"
  }
}

import com.intellij.openapi.actionSystem.IdeActions

@RunWith(classOf[AllTests])
class HoconSubstitutionResolutionTest extends HoconNavigationTest(IdeActions.ACTION_GOTO_DECLARATION, "substitution")
object HoconSubstitutionResolutionTest extends TestSuiteCompanion[HoconSubstitutionResolutionTest]

@RunWith(classOf[AllTests])
class HoconGoToNextTest extends HoconNavigationTest("HoconGotoNext", "gotoNext")
object HoconGoToNextTest extends TestSuiteCompanion[HoconGoToNextTest]

@RunWith(classOf[AllTests])
class HoconGoToPrevTest extends HoconNavigationTest("HoconGotoPrev", "gotoPrev")
object HoconGoToPrevTest extends TestSuiteCompanion[HoconGoToPrevTest]
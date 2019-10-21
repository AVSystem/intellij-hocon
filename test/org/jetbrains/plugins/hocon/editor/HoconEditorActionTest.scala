package org.jetbrains.plugins.hocon
package editor

import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.actionSystem.IdeActions._
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.actionSystem.EditorActionManager
import com.intellij.psi.PsiFile
import com.intellij.testFramework.EditorTestUtil
import org.junit.Assert.assertNotNull
import org.junit.runner.RunWith
import org.junit.runners.AllTests

abstract class HoconEditorActionTest(actionId: String, subPath: String)
  extends HoconActionTest(actionId, subPath) {

  override protected def executeAction(dataContext: DataContext, editor: Editor): Unit = {
    val actionHandler = EditorActionManager.getInstance.getActionHandler(actionId)
    assertNotNull(actionHandler)

    val caretModel = editor.getCaretModel
    inWriteCommandAction {
      actionHandler.execute(editor, caretModel.getCurrentCaret, dataContext)
    }
  }

  protected def extractResult(file: PsiFile, editor: Editor): String = {
    val fileText = editor.getDocument.getText

    val caretModel = editor.getCaretModel
    caretModel.getOffset match {
      case offset if (0 to fileText.length).contains(offset) =>
        fileText.substring(0, offset) + EditorTestUtil.CARET_TAG + fileText.substring(offset)
      case _ => fileText
    }
  }
}

@RunWith(classOf[AllTests])
class HoconMoveStatementDownActionTest
  extends HoconEditorActionTest(ACTION_MOVE_STATEMENT_DOWN_ACTION, "moveStatement/both")
object HoconMoveStatementDownActionTest extends TestSuiteCompanion[HoconMoveStatementDownActionTest]

@RunWith(classOf[AllTests])
class HoconMoveStatementDownOnlyActionTest
  extends HoconEditorActionTest(ACTION_MOVE_STATEMENT_DOWN_ACTION, "moveStatement/down")
object HoconMoveStatementDownOnlyActionTest extends TestSuiteCompanion[HoconMoveStatementDownOnlyActionTest]

@RunWith(classOf[AllTests])
class HoconMoveStatementUpActionTest
  extends HoconEditorActionTest(ACTION_MOVE_STATEMENT_UP_ACTION, "moveStatement/up")
object HoconMoveStatementUpActionTest extends TestSuiteCompanion[HoconMoveStatementUpActionTest]

@RunWith(classOf[AllTests])
class HoconEnterActionTest extends HoconEditorActionTest(ACTION_EDITOR_ENTER, "enter")
object HoconEnterActionTest extends TestSuiteCompanion[HoconEnterActionTest]

@RunWith(classOf[AllTests])
class HoconJoinLinesTest extends HoconEditorActionTest(ACTION_EDITOR_JOIN_LINES, "joinLines")
object HoconJoinLinesTest extends TestSuiteCompanion[HoconJoinLinesTest]

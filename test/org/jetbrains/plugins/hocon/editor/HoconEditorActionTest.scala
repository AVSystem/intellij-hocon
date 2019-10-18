package org.jetbrains.plugins.hocon
package editor

import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.actionSystem.EditorActionManager
import com.intellij.psi.PsiFile
import com.intellij.testFramework.EditorTestUtil
import org.junit.Assert.assertNotNull

abstract class HoconEditorActionTest protected(
  override protected val actionId: String,
  subPath: String
) extends HoconActionTest(actionId, subPath) {

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

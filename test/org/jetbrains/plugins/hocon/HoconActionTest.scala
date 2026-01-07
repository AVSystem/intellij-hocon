package org.jetbrains.plugins.hocon

import com.intellij.ide.DataManager
import com.intellij.openapi.actionSystem.*
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.fileEditor.{FileEditorManager, OpenFileDescriptor}
import com.intellij.psi.PsiFile
import com.intellij.testFramework.TestActionEvent
import org.junit.Assert.assertNotNull

import scala.annotation.unused

/** @author
  *   ghik
  */
abstract class HoconActionTest protected (protected val actionId: String, subPath: String)
  extends HoconFileSetTestCase(s"actions/$subPath") {

  // Code based on AbstractEnterActionTestBase

  import HoconActionTest.*
  import HoconFileSetTestCase.*

  override protected def transform(data: Seq[String]): String = {
    val (fileText, offset) = extractCaret(data.head)
    val psiFile = createPseudoPhysicalHoconFile(fileText)

    val editorManager = FileEditorManager.getInstance(myProject)
    val editor: Editor =
      editorManager.openTextEditor(new OpenFileDescriptor(myProject, psiFile.getVirtualFile, 0), false)
    assertNotNull(editor)
    editor.getCaretModel.moveToOffset(offset)

    try {
      executeAction(mockDataContext(psiFile, editor), editor)
      extractResult(psiFile, editor)
    } finally {
      editorManager.closeFile(psiFile.getVirtualFile)
    }
  }

  protected def executeAction(dataContext: DataContext, @unused editor: Editor): Unit = {
    val action = ActionManager.getInstance.getAction(actionId)
    val actionEvent = TestActionEvent.createTestEvent(action, dataContext)

    actionEvent.getPresentation match {
      case presentation if presentation.isEnabled && presentation.isVisible =>
        action.actionPerformed(actionEvent)
      case _ =>
    }
  }

  protected def extractResult(file: PsiFile, editor: Editor): String
}

object HoconActionTest {

  private def mockDataContext(file: PsiFile, editor: Editor) = {
    val parentContext = DataManager.getInstance().getDataContext(editor.getComponent)
    CustomizedDataContext.withSnapshot(
      parentContext,
      (sink: DataSink) => {
        sink.set(CommonDataKeys.PROJECT, file.getProject)
        sink.set(CommonDataKeys.EDITOR, editor)
        sink.set(CommonDataKeys.PSI_FILE, file)
      },
    )
  }

}

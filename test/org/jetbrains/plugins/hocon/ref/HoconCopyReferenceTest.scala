package org.jetbrains.plugins.hocon
package ref

import java.awt.datatransfer.DataFlavor.stringFlavor

import com.intellij.openapi.actionSystem.IdeActions.ACTION_COPY_REFERENCE
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.ide.CopyPasteManager
import com.intellij.psi.PsiFile

class HoconCopyReferenceTest extends HoconActionTest(ACTION_COPY_REFERENCE, "copyReference") {
  protected def extractResult(file: PsiFile, editor: Editor): String =
    CopyPasteManager.getInstance.getContents
      .getTransferData(stringFlavor)
      .asInstanceOf[String]
}
object HoconCopyReferenceTest extends TestSuiteCompanion[HoconCopyReferenceTest]

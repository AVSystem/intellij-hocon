package org.jetbrains.plugins.hocon
package ref

import com.intellij.ide.actions.QualifiedNameProvider
import com.intellij.openapi.editor.{Editor, EditorModificationUtil}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.hocon.psi.HKey

/**
  * @author ghik
  */
class HoconQualifiedNameProvider extends QualifiedNameProvider {
  def adjustElementToCopy(element: PsiElement): PsiElement = element

  def getQualifiedName(element: PsiElement): String = element match {
    case key: HKey => key.fullValidContainingPath match {
      case Some((_, path)) => path.mkString(".")
      case _ => null
    }
    case _ => null
  }

  override def insertQualifiedName(fqn: String, element: PsiElement, editor: Editor, project: Project): Unit =
    EditorModificationUtil.insertStringAtCaret(editor, fqn)

  def qualifiedNameToElement(fqn: String, project: Project): PsiElement = null
}

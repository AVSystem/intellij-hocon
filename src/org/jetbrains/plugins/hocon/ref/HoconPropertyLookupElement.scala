package org.jetbrains.plugins.hocon
package ref

import com.intellij.codeInsight.lookup.{LookupElement, LookupElementPresentation}
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.hocon.semantics.ResolvedField

class HoconPropertyLookupElement(resField: ResolvedField) extends LookupElement {
  def getLookupString: String = resField.field.key.fold("")(_.getText)

  override def renderElement(presentation: LookupElementPresentation): Unit = {
    super.renderElement(presentation)
    presentation.setIcon(PropertyIcon)
  }

  override def getObject: ResolvedField = resField

  override def getPsiElement: PsiElement = resField.field
}

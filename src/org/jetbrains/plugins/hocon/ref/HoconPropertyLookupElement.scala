package org.jetbrains.plugins.hocon
package ref

import com.intellij.codeInsight.lookup.{LookupElement, LookupElementPresentation}
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.hocon.semantics._

class HoconPropertyLookupElement(resField: ResolvedField) extends LookupElement {
  def getLookupString: String = resField.field.key.fold("")(_.getText)

  override def renderElement(presentation: LookupElementPresentation): Unit = {
    super.renderElement(presentation)
    presentation.setIcon(PropertyIcon)
    val resolvedValue = resField.resolveValue
    if (resolvedValue != ObjectValue) {
      presentation.setTailText(resolvedValue.valueHint)
    } else {
      presentation.setItemTextBold(true)
    }
    presentation.setTypeText(resolvedValue.typeHint)
  }

  override def getObject: ResolvedField = resField

  override def getPsiElement: PsiElement = resField.hkey

  def repr: String = {
    val resValue = resField.resolveValue
    s"$getLookupString (${resValue.typeHint})${resValue.valueHint}"
  }
}

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
    resField.resolveValue match {
      case NullValue =>
        presentation.setTailText(" = null")
        presentation.setTypeText("null")
      case BooleanValue(value) =>
        presentation.setTailText(s" = $value")
        presentation.setTypeText("boolean")
      case NumberValue(value) =>
        presentation.setTailText(s" = $value")
        presentation.setTypeText("number")
      case sv: StringValue =>
        presentation.setTailText(s" = ${sv.quotedIfNecessary}")
        presentation.setTypeText("string")
      case ArrayValue =>
        presentation.setTypeText("array")
      case ObjectValue =>
        presentation.setTypeText("object")
        presentation.setItemTextBold(true)
      case _ =>
    }
  }

  override def getObject: ResolvedField = resField

  override def getPsiElement: PsiElement = resField.field
}

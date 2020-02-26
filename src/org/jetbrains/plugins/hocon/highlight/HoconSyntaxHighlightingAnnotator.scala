package org.jetbrains.plugins.hocon
package highlight

import com.intellij.lang.annotation.{AnnotationHolder, Annotator, HighlightSeverity}
import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.psi.PsiElement

class HoconSyntaxHighlightingAnnotator extends Annotator {

  import org.jetbrains.plugins.hocon.lexer.HoconTokenType._
  import org.jetbrains.plugins.hocon.parser.HoconElementSets._
  import org.jetbrains.plugins.hocon.parser.HoconElementType._

  def annotate(element: PsiElement, holder: AnnotationHolder): Unit = {
    lazy val parentType = element.getParent.getNode.getElementType
    lazy val firstChildType = element.getFirstChild.getNode.getElementType

    def annot(attrs: TextAttributesKey): Unit =
      holder.newSilentAnnotation(HighlightSeverity.INFORMATION).range(element).textAttributes(attrs).create()

    element.getNode.getElementType match {
      case Null =>
        annot(HoconHighlighterColors.Null)

      case Boolean =>
        annot(HoconHighlighterColors.Boolean)

      case Number =>
        annot(HoconHighlighterColors.Number)

      case UnquotedChars if parentType == Include =>
        annot(HoconHighlighterColors.Include)

      case UnquotedChars if parentType == Included || parentType == QualifiedIncluded =>
        annot(HoconHighlighterColors.IncludeModifier)

      case LParen | RParen if parentType == Included || parentType == QualifiedIncluded =>
        annot(HoconHighlighterColors.IncludeModifierParens)

      case KeyPart if firstChildType == UnquotedString =>
        val textAttributesKey = element.getParent.getParent.getNode.getElementType match {
          case Path => HoconHighlighterColors.SubstitutionKey
          case KeyedField.extractor() => HoconHighlighterColors.EntryKey
        }
        annot(textAttributesKey)

      case Period if parentType == Path || parentType == PrefixedField =>
        annot(HoconHighlighterColors.PathSeparator)

      case _ =>
    }

  }
}

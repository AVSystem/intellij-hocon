package org.jetbrains.plugins.hocon
package parser

import com.intellij.lang.ASTNode
import com.intellij.lang.annotation.{AnnotationHolder, Annotator, HighlightSeverity}
import com.intellij.lexer.StringLiteralLexer
import com.intellij.psi.tree.IElementType
import com.intellij.psi.{PsiElement, StringEscapesTokenTypes, TokenType}

import scala.annotation.tailrec

class HoconErrorHighlightingAnnotator extends Annotator {

  import org.jetbrains.plugins.hocon.lexer.HoconTokenType._
  import org.jetbrains.plugins.hocon.parser.HoconElementType._

  def annotate(element: PsiElement, holder: AnnotationHolder): Unit = {
    element.getNode.getElementType match {
      case QuotedString =>
        val lexer = new StringLiteralLexer('"', QuotedString)
        lexer.start(element.getText)

        Iterator.continually {
          val range = TextRange(lexer.getTokenStart, lexer.getTokenEnd)
            .shiftRight(element.getTextRange.getStartOffset)
          val result = (lexer.getTokenType, range)
          lexer.advance()
          result
        }.takeWhile {
          case (tokenType, _) => tokenType != null
        } foreach {
          case (StringEscapesTokenTypes.INVALID_CHARACTER_ESCAPE_TOKEN, range) =>
            holder.newAnnotation(HighlightSeverity.ERROR, "invalid escape character").range(range).create()
          case (StringEscapesTokenTypes.INVALID_UNICODE_ESCAPE_TOKEN, range) =>
            holder.newAnnotation(HighlightSeverity.ERROR, "invalid unicode escape").range(range).create()
          case _ =>
        }

      case Concatenation =>
        @tailrec
        def validateConcatenation(constrainingToken: IElementType, child: ASTNode): Unit = if (child != null) {
          (constrainingToken, child.getElementType) match {
            case (_, Substitution | BadCharacter | TokenType.ERROR_ELEMENT | TokenType.WHITE_SPACE) =>

              validateConcatenation(constrainingToken, child.getTreeNext)

            case (StringValue, StringValue) |
                 (Object, Object) |
                 (Array, Array) |
                 (null, _) =>

              validateConcatenation(child.getElementType, child.getTreeNext)

            case (required, actual) =>

              val msg = s"cannot concatenate ${uncaps(required.toString)} with ${uncaps(actual.toString)}"
              holder.newAnnotation(HighlightSeverity.ERROR, msg).range(child).create()
              validateConcatenation(actual, child.getTreeNext)

          }
        }

        validateConcatenation(null, element.getNode.getFirstChildNode)

      case _ =>

    }
  }
}

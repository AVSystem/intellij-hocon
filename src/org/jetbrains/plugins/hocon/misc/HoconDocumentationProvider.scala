package org.jetbrains.plugins.hocon
package misc

import com.intellij.lang.documentation.{DocumentationMarkup, DocumentationProvider}
import com.intellij.psi.PsiElement
import org.apache.commons.lang3.StringEscapeUtils
import org.jetbrains.plugins.hocon.psi._

class HoconDocumentationProvider extends DocumentationProvider {
  private def key(origElem: PsiElement): Option[HKey] = origElem match {
    case key: HKey => Some(key)
    case _ => origElem.parentOfType[HKey]
  }

  override def getQuickNavigateInfo(element: PsiElement, originalElement: PsiElement): String =
    key(originalElement).flatMap(_.fullValidContainingPath).fold(null: String) {
      case (_, path) => path.mkString(".")
    }

  override def generateDoc(element: PsiElement, originalElement: PsiElement): String =
    key(originalElement).fold(null: String) { key =>
      val resField = key.parent match {
        case path: HPath => path.resolve()
        case kf: HKeyedField => kf.makeContext
      }
      def findDoc(rfOpt: Option[ResolvedField]): String =
        rfOpt.fold(null: String) { rf =>
          rf.field match {
            case vf: HValuedField =>
              val docComments = vf.enclosingObjectField.docComments
              if (docComments.isEmpty) findDoc(rf.nextOccurrence(reverse = true))
              else {
                val fullPath = vf.key.flatMap(_.fullValidContainingPath)
                  .map({ case (_, path) => path.mkString(".") }).getOrElse("")
                val definition = DocumentationMarkup.DEFINITION_START + fullPath + DocumentationMarkup.DEFINITION_END
                val content = docComments.map(c => StringEscapeUtils.escapeHtml4(c.getText.stripPrefix("#")))
                  .mkString(DocumentationMarkup.CONTENT_START, "<br/>", DocumentationMarkup.CONTENT_END)
                definition + content
              }
            case _ =>
              findDoc(rf.nextOccurrence(reverse = true))
          }
        }
      findDoc(resField)
    }
}

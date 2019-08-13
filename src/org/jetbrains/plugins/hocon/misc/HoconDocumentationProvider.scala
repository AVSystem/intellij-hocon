package org.jetbrains.plugins.hocon
package misc

import com.intellij.lang.documentation.{DocumentationMarkup, DocumentationProviderEx}
import com.intellij.openapi.editor.Editor
import com.intellij.psi.{PsiElement, PsiFile, PsiManager}
import org.apache.commons.lang3.StringEscapeUtils
import org.jetbrains.plugins.hocon.psi._

class HoconDocumentationProvider extends DocumentationProviderEx {
  private def key(origElem: PsiElement): Option[HKey] = origElem match {
    case key: HKey => Some(key)
    case _ => origElem.parentOfType[HKey]
  }

  private def findDocSource(rfOpt: Option[ResolvedField]): Option[HValuedField] = rfOpt.flatMap { rf =>
    rf.field match {
      case vf: HValuedField if vf.enclosingObjectField.docComments.nonEmpty => Some(vf)
      case _ => findDocSource(rf.nextOccurrence(reverse = true))
    }
  }

  override def getDocumentationElementForLookupItem(psiManager: PsiManager, obj: Any, element: PsiElement): PsiElement =
    obj match {
      case rf: ResolvedField => // see HoconFieldLookupElement.getObject
        findDocSource(Some(rf)).getOrElse(rf.field)
      case _ =>
        null
    }

  override def getCustomDocumentationElement(editor: Editor, file: PsiFile, contextElement: PsiElement): PsiElement =
    key(contextElement).nullOr { key =>
      val resField = key.resolved
      findDocSource(resField).orElse(resField.map(_.field)).orNull
    }

  override def getQuickNavigateInfo(element: PsiElement, originalElement: PsiElement): String =
    key(originalElement).flatMap(_.fullValidContainingPath).fold(null: String) {
      case (_, path) => path.mkString(".")
    }

  override def generateDoc(element: PsiElement, originalElement: PsiElement): String = element match {
    case vf: HValuedField =>
      val fullPath = vf.key.flatMap(_.fullValidContainingPath)
        .map({ case (_, path) => path.mkString(".") }).getOrElse("")
      val definition = DocumentationMarkup.DEFINITION_START + fullPath + DocumentationMarkup.DEFINITION_END
      val content = vf.enclosingObjectField.docComments
        .map(c => StringEscapeUtils.escapeHtml4(c.getText.stripPrefix("#")))
        .mkString(DocumentationMarkup.CONTENT_START, "<br/>", DocumentationMarkup.CONTENT_END)
      definition + content
    case _ => null
  }
}

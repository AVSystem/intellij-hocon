package org.jetbrains.plugins.hocon
package misc

import com.intellij.lang.documentation.{DocumentationMarkup, DocumentationProviderEx}
import com.intellij.openapi.editor.Editor
import com.intellij.psi.{PsiElement, PsiFile, PsiManager}
import org.apache.commons.lang3.StringEscapeUtils
import org.jetbrains.plugins.hocon.psi._

import scala.annotation.tailrec

class HoconDocumentationProvider extends DocumentationProviderEx {
  private def key(origElem: PsiElement): Option[HKey] = origElem match {
    case key: HKey => Some(key)
    case _ => origElem.parentOfType[HKey]
  }

  @tailrec private def findDocSource(rfOpt: Option[ResolvedField]): Option[HValuedField] = rfOpt match {
    case Some(rf) => rf.field match {
      case vf: HValuedField if vf.enclosingObjectField.docComments.nonEmpty => Some(vf)
      case _ => findDocSource(rf.nextOccurrence(ResOpts(reverse = true)))
    }
    case None => None
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
    key(originalElement).flatMap(_.fullPathText).orNull

  override def generateDoc(element: PsiElement, originalElement: PsiElement): String = element match {
    case vf: HValuedField =>
      val docComments = vf.enclosingObjectField.docComments
      if (docComments.nonEmpty) {
        val fullPath = vf.key.flatMap(_.fullPathText).getOrElse("")
        val definition = DocumentationMarkup.DEFINITION_START + fullPath + DocumentationMarkup.DEFINITION_END
        val content = docComments
          .map(c => StringEscapeUtils.escapeHtml4(c.getText.stripPrefix("#")))
          .mkString(DocumentationMarkup.CONTENT_START, "<br/>", DocumentationMarkup.CONTENT_END)
        definition + content
      } else null
    case _ => null
  }
}

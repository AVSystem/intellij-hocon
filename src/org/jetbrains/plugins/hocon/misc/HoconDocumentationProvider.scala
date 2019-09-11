package org.jetbrains.plugins.hocon
package misc

import com.intellij.lang.documentation.{DocumentationMarkup, DocumentationProviderEx}
import com.intellij.openapi.editor.Editor
import com.intellij.psi.{PsiElement, PsiFile, PsiManager}
import org.apache.commons.lang3.StringEscapeUtils
import org.jetbrains.plugins.hocon.psi._
import org.jetbrains.plugins.hocon.semantics.{ResOpts, ResolvedField}

import scala.annotation.tailrec

class HoconDocumentationProvider extends DocumentationProviderEx {
  private def key(origElem: PsiElement): Option[HKey] = origElem match {
    case key: HKey => Some(key)
    case _ => origElem.parentOfType[HKey]
  }

  @tailrec private def findDocField(rfOpt: Option[ResolvedField]): Option[HValuedField] = rfOpt match {
    case Some(rf) => rf.field match {
      case vf: HValuedField if vf.enclosingObjectField.docComments.nonEmpty => Some(vf)
      case _ => findDocField(rf.nextOccurrence(ResOpts(reverse = true)))
    }
    case None => None
  }

  override def getDocumentationElementForLookupItem(psiManager: PsiManager, obj: Any, element: PsiElement): PsiElement =
    obj match {
      case rf: ResolvedField => rf.hkey // see HoconPropertyLookupElement.getObject
      case _ => null
    }

  override def getCustomDocumentationElement(editor: Editor, file: PsiFile, contextElement: PsiElement): PsiElement =
    key(contextElement).orNull

  override def getQuickNavigateInfo(element: PsiElement, originalElement: PsiElement): String = {
    val res = for {
      hkey <- key(originalElement)
      fullPathText <- hkey.fullPathText
    } yield fullPathText + hkey.resolved.fold("")(_.resolveValue.valueHint)
    res.orNull
  }

  override def generateDoc(element: PsiElement, originalElement: PsiElement): String = {
    import DocumentationMarkup._
    val res = for {
      hkey <- element.opt.collectOnly[HKey]
      resolved <- hkey.resolved
    } yield {
      val docField = findDocField(Some(resolved)).getOrElse(resolved.field)
      val fullPath = docField.key.flatMap(_.fullPathText).getOrElse("")
      val hintString = resolved.resolveValue.valueHint
      val hintRepr = if (hintString.nonEmpty) s"$GRAYED_START$hintString$GRAYED_END" else ""
      val definition = s"$DEFINITION_START$fullPath$hintRepr$DEFINITION_END"
      val docComments = docField.enclosingObjectField.docComments
      val content =
        if (docComments.isEmpty) ""
        else docComments
          .map(c => StringEscapeUtils.escapeHtml4(c.getText.stripPrefix("#")))
          .mkString(CONTENT_START, "<br/>", CONTENT_END)
      definition + content
    }
    res.orNull
  }
}

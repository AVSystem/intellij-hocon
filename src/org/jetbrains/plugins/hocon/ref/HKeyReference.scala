package org.jetbrains.plugins.hocon.ref

import com.intellij.openapi.util.TextRange
import com.intellij.psi.{ElementManipulators, PsiElement, PsiReference}
import org.jetbrains.plugins.hocon.CommonUtil._
import org.jetbrains.plugins.hocon.psi.{HKey, HPath}

/**
 * @author ghik
 */
class HKeyReference(key: HKey) extends PsiReference {
  def getCanonicalText: String = key.stringValue

  def getElement: PsiElement = key

  def isReferenceTo(element: PsiElement): Boolean =
    element == resolve()

  def bindToElement(element: PsiElement): PsiElement = null

  def handleElementRename(newElementName: String): PsiElement =
    ElementManipulators.getManipulator(key).handleContentChange(key, newElementName)

  def isSoft = true

  def getRangeInElement: TextRange = ElementManipulators.getValueTextRange(key)

  def resolve(): PsiElement = key.parent match {
    case path: HPath =>
      val result = path.resolveAsSelfReference orElse
        path.allValidKeys.flatMap(keys =>
          path.getContainingFile.toplevelEntries.occurrences(keys, reverse = true).nextOption)
      result.flatMap(_.key).getOrElse(key)
    case _ => key
  }
}

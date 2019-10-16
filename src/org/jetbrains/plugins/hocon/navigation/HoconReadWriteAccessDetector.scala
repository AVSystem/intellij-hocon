package org.jetbrains.plugins.hocon
package navigation

import com.intellij.codeInsight.highlighting.ReadWriteAccessDetector
import com.intellij.codeInsight.highlighting.ReadWriteAccessDetector.Access
import com.intellij.psi.{PsiElement, PsiReference}
import org.jetbrains.plugins.hocon.psi.{HFieldKey, HKey}

class HoconReadWriteAccessDetector extends ReadWriteAccessDetector {
  def isReadWriteAccessible(element: PsiElement): Boolean = element match {
    case _: HKey => true
    case _ => false
  }

  def isDeclarationWriteAccess(element: PsiElement): Boolean = false

  def getReferenceAccess(referencedElement: PsiElement, reference: PsiReference): Access =
    getExpressionAccess(reference.getElement)

  def getExpressionAccess(expression: PsiElement): Access = expression match {
    case _: HFieldKey => Access.Write
    case _ => Access.Read
  }
}

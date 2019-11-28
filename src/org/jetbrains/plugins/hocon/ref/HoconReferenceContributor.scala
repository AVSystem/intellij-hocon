package org.jetbrains.plugins.hocon
package ref

import com.intellij.patterns.{PlatformPatterns, PsiElementPattern}
import com.intellij.psi.{PsiElement, PsiReferenceContributor, PsiReferenceRegistrar}
import org.jetbrains.plugins.hocon.psi.{HIncludeTarget, HStringValue}

import scala.reflect.{ClassTag, classTag}

class HoconReferenceContributor extends PsiReferenceContributor {
  private def pattern[T <: PsiElement : ClassTag]: PsiElementPattern.Capture[T] =
    PlatformPatterns.psiElement(classTag[T].runtimeClass.asInstanceOf[Class[T]])

  def registerReferenceProviders(registrar: PsiReferenceRegistrar): Unit = {
    registrar.registerReferenceProvider(pattern[HIncludeTarget], new IncludedFileReferenceProvider)
    registrar.registerReferenceProvider(pattern[HStringValue], new HoconPropertiesReferenceProvider)
  }
}

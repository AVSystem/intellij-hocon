package org.jetbrains.plugins.hocon
package ref

import com.intellij.patterns.{PlatformPatterns, PsiElementPattern}
import com.intellij.psi.{PsiElement, PsiLiteral, PsiReferenceContributor, PsiReferenceRegistrar}
import org.jetbrains.plugins.hocon.psi.{HIncludeTarget, HKeyPart}

import scala.reflect.{ClassTag, classTag}

class HoconReferenceContributor extends PsiReferenceContributor {
  private def pattern[T <: PsiElement : ClassTag]: PsiElementPattern.Capture[T] =
    PlatformPatterns.psiElement(classTag[T].runtimeClass.asInstanceOf[Class[T]])

  def registerReferenceProviders(registrar: PsiReferenceRegistrar): Unit = {
    registrar.registerReferenceProvider(
      pattern[HIncludeTarget],
      new IncludedFileReferenceProvider
    )
    registrar.registerReferenceProvider(
      // don't inject HOCON refs into HOCON keys or includes (hence not HString)
      pattern[PsiLiteral] andNot pattern[HIncludeTarget] andNot pattern[HKeyPart],
      new HoconPropertiesReferenceProvider
    )
  }
}

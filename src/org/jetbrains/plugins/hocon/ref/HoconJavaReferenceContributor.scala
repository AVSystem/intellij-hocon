package org.jetbrains.plugins.hocon
package ref

import com.intellij.patterns.PlatformPatterns
import com.intellij.psi.{PsiReferenceContributor, PsiReferenceRegistrar}
import org.jetbrains.plugins.hocon.psi.{HString, HStringValue}

class HoconJavaReferenceContributor extends PsiReferenceContributor {
  override def registerReferenceProviders(registrar: PsiReferenceRegistrar): Unit = {
    registrar.registerReferenceProvider(
      PlatformPatterns.psiElement(classOf[HString]),
      new HStringJavaClassReferenceProvider
    )

    registrar.registerReferenceProvider(
      PlatformPatterns.psiElement(classOf[HStringValue]),
      new HoconBeanReferenceProvider
    )
  }
}

package org.jetbrains.plugins.hocon
package ref

import com.intellij.patterns.PlatformPatterns
import com.intellij.psi.{PsiReferenceContributor, PsiReferenceRegistrar}
import org.jetbrains.plugins.hocon.psi.HString

class HoconJavaReferenceContributor extends PsiReferenceContributor {
  override def registerReferenceProviders(registrar: PsiReferenceRegistrar): Unit = {
    val pattern = PlatformPatterns.psiElement(classOf[HString])
    registrar.registerReferenceProvider(pattern, new HStringJavaClassReferenceProvider)
  }
}

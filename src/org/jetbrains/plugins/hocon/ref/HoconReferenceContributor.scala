package org.jetbrains.plugins.hocon.ref

import com.intellij.patterns.PlatformPatterns
import com.intellij.psi.{PsiReferenceContributor, PsiReferenceRegistrar}
import org.jetbrains.plugins.hocon.psi.{HQualifiedIncluded, HString}

class HoconReferenceContributor extends PsiReferenceContributor {
  def registerReferenceProviders(registrar: PsiReferenceRegistrar): Unit = {
    val hStringPattern = PlatformPatterns.psiElement(classOf[HString])
    try {
      getClass.getClassLoader.loadClass("com.intellij.psi.impl.source.resolve.reference.impl.providers.JavaClassReferenceProvider")
      registrar.registerReferenceProvider(hStringPattern, new HStringJavaClassReferenceProvider)
    } catch {
      case _: ClassNotFoundException => // no java support, skip java reference provider
    }
    registrar.registerReferenceProvider(hStringPattern.withParent(classOf[HQualifiedIncluded]), new IncludedFileReferenceProvider)
  }
}

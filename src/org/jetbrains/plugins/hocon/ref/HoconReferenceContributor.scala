package org.jetbrains.plugins.hocon.ref

import com.intellij.patterns.{PlatformPatterns, PsiElementPattern}
import com.intellij.psi.{PsiReferenceContributor, PsiReferenceRegistrar}
import org.jetbrains.plugins.hocon.psi.{HQualifiedIncluded, HString}

class HoconReferenceContributor extends PsiReferenceContributor {
  import HoconReferenceContributor._

  def registerReferenceProviders(registrar: PsiReferenceRegistrar): Unit = {
    registrar.registerReferenceProvider(hStringPattern.withParent(classOf[HQualifiedIncluded]), new IncludedFileReferenceProvider)
  }
}

object HoconReferenceContributor {
  val hStringPattern: PsiElementPattern.Capture[HString] = PlatformPatterns.psiElement(classOf[HString])
}

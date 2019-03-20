package org.jetbrains.plugins.hocon.ref

import com.intellij.psi.{PsiReferenceContributor, PsiReferenceRegistrar}

class HoconJavaReferenceContributor extends PsiReferenceContributor {
  import HoconReferenceContributor._

  override def registerReferenceProviders(registrar: PsiReferenceRegistrar): Unit = {
    registrar.registerReferenceProvider(hStringPattern, new HStringJavaClassReferenceProvider)
  }
}

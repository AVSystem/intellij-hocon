package org.jetbrains.plugins.hocon
package ref

import com.intellij.lang.Language
import com.intellij.patterns.{PlatformPatterns, PsiElementPattern}
import com.intellij.psi.{PsiElement, PsiLanguageInjectionHost, PsiLiteral, PsiReferenceContributor, PsiReferenceRegistrar}
import org.jetbrains.plugins.hocon.psi.HString

import scala.reflect.{classTag, ClassTag}

class HoconJavaReferenceContributor extends PsiReferenceContributor {
  private def pattern[T <: PsiElement: ClassTag]: PsiElementPattern.Capture[T] =
    PlatformPatterns.psiElement(classTag[T].runtimeClass.asInstanceOf[Class[T]])

  override def registerReferenceProviders(registrar: PsiReferenceRegistrar): Unit = {
    registrar.registerReferenceProvider(pattern[HString], new HStringJavaClassReferenceProvider)
    // Java, Scala and Groovy string literals
    registrar.registerReferenceProvider(pattern[PsiLiteral], new HoconPropertiesReferenceProvider)
    // Kotlin string templates don't implement PsiLiteral. The pattern must be restricted to Kotlin, otherwise property
    // references would be injected into string literals of every language, e.g. TOML (#87).
    Language.findLanguageByID("kotlin").opt.foreach { kotlin =>
      registrar.registerReferenceProvider(
        pattern[PsiLanguageInjectionHost].withLanguage(kotlin),
        new HoconPropertiesReferenceProvider,
      )
    }
  }
}

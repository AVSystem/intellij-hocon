package org.jetbrains.plugins.hocon
package ref

import com.intellij.psi.PsiClass
import org.junit.Assert.assertEquals

class JavaClassReferenceTest extends HoconSingleModuleTest {
  def rootPath: String = "testdata/javaClassRefs"

  def testReferencesInJavaStringLiteral(): Unit = {
    val javaFile = psiManager.findFile(findVirtualFile("pkg/Main.java"))
    val cls = javaFile.depthFirst.collectFirst({ case cls: PsiClass => cls }).get

    val hoconFile = psiManager.findFile(findVirtualFile("application.conf"))
    val ref = hoconFile.findReferenceAt(11)

    assertEquals(cls, ref.resolve())
  }
}

package org.jetbrains.plugins.hocon
package ref

import com.intellij.psi.PsiLiteralExpression
import org.jetbrains.plugins.hocon.psi.HKey
import org.junit.Assert.assertEquals

class JavaLiteralHoconReferenceTest extends HoconSingleModuleTest {
  def rootPath: String = "testdata/javaLiteralRefs"

  def testReferencesInJavaStringLiteral(): Unit = {
    val offsets = List(0, 5, 11)

    val hoconFile = psiManager.findFile(findVirtualFile("application.conf"))
    val expectedKeys = offsets.map(off => hoconFile.findElementAt(off).parentOfType[HKey].get)

    val javaFile = psiManager.findFile(findVirtualFile("pkg/Main.java"))
    val litOffset = javaFile.depthFirst.collectFirst({ case lit: PsiLiteralExpression => lit }).get.getTextOffset + 1
    val resolved = offsets.map(off => javaFile.findReferenceAt(litOffset + off).resolve())

    assertEquals(expectedKeys, resolved)
  }
}

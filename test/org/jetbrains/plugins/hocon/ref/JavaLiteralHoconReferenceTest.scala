package org.jetbrains.plugins.hocon
package ref

import com.intellij.json.psi.JsonStringLiteral
import com.intellij.psi.PsiLiteralExpression
import org.jetbrains.plugins.hocon.psi.HKey
import org.junit.Assert.{assertEquals, assertTrue}

class JavaLiteralHoconReferenceTest extends HoconSingleModuleTest {
  def rootPath: String = "testdata/javaLiteralRefs"

  def testReferencesInJavaStringLiteral(): Unit = {
    val offsets = List(0, 5, 11)

    val hoconFile = psiManager.findFile(findVirtualFile("application.conf"))
    val expectedKeys = offsets.map(off => hoconFile.findElementAt(off).parentOfType[HKey].get)

    val javaFile = psiManager.findFile(findVirtualFile("pkg/Main.java"))
    val litOffset = javaFile.depthFirst.collectFirst { case lit: PsiLiteralExpression => lit }.get.getTextOffset + 1
    val resolved = offsets.map(off => javaFile.findReferenceAt(litOffset + off).resolve())

    assertEquals(expectedKeys, resolved)
  }

  def testReferencesInKotlinStringLiteral(): Unit = {
    val offsets = List(0, 5, 11)

    val hoconFile = psiManager.findFile(findVirtualFile("application.conf"))
    val expectedKeys = offsets.map(off => hoconFile.findElementAt(off).parentOfType[HKey].get)

    // Kotlin PSI classes are not on the test compile classpath, so locate the literal by its text
    val kotlinFile = psiManager.findFile(findVirtualFile("pkg/Main.kt"))
    val litOffset = kotlinFile.getText.indexOf("this.thing.here")
    val resolved = offsets.map(off => kotlinFile.findReferenceAt(litOffset + off).resolve())

    assertEquals(expectedKeys, resolved)
  }

  // https://github.com/AVSystem/intellij-hocon/issues/87
  def testNoReferencesInOtherLanguagesStringLiteral(): Unit = {
    val jsonFile = psiManager.findFile(findVirtualFile("catalog.json"))
    val literal = jsonFile.depthFirst.collectFirst {
      case lit: JsonStringLiteral if lit.getValue.contains('.') => lit
    }.get

    assertTrue(literal.getReferences.collectFirst { case ref: HoconPropertyReference => ref }.isEmpty)
  }
}

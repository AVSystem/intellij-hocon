package org.jetbrains.plugins.hocon
package completion

import com.intellij.codeInsight.completion.CompletionType
import com.intellij.testFramework.fixtures.BasePlatformTestCase
import org.jetbrains.plugins.hocon.ref.HoconPropertyLookupElement
import org.junit.Assert.assertEquals

class HoconCompletionTest extends BasePlatformTestCase {

  override def getTestDataPath: String = "testdata/completion"

  def testToplevel(): Unit = testCompletion(
    "moar (number) = 5",
    "top (object) = {...}",
  )

  def testInner(): Unit = testCompletion(
    "arr (array) = [...]",
    "boo (boolean) = true",
    "nul (null) = null",
    "num (number) = 2",
    "obj (object) = {...}",
    "str (string) = kek",
  )

  def testInner2(): Unit = testInner()

  def testToplevelSub(): Unit = testCompletion(
    "value ()",
    "moar (number) = 5",
    "top (object) = {...}",
  )

  def testInnerSub(): Unit = testInner()

  private def testCompletion(expected: String*): Unit = {
    myFixture.configureByFiles(s"${getTestName(true)}.conf", "included.conf")
    val lookups = myFixture.complete(CompletionType.BASIC) map {
      case hple: HoconPropertyLookupElement => hple.repr
    }
    assertEquals(lookups.toSeq, expected)
  }
}

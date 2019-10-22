package org.jetbrains.plugins.hocon
package usages

import com.intellij.openapi.editor.LogicalPosition
import com.intellij.psi.search.ProjectScope
import com.intellij.psi.{PsiClass, PsiElement}
import com.intellij.testFramework.fixtures.impl.CodeInsightTestFixtureImpl
import org.jetbrains.plugins.hocon.psi.HKey
import org.junit.Assert.assertEquals

import scala.reflect.ClassTag

class HoconFindUsagesTest extends HoconMultiModuleTest {
  def rootPath: String = "testdata/findUsages"

  override def moduleDependencies: Seq[(String, String)] = List("modA" -> "modB")

  private val expectedHoconUsages =
    """modA/lib/reference.conf:3:5
      |modA/lib/reference.conf:6:21
      |modA/lib/reference.conf:7:17
      |modA/libsrc/libpkg/LibMain.java:4:44
      |modA/src/application.conf:1:11
      |modA/src/application.conf:2:20
      |modA/src/application.conf:3:17
      |modA/src/pkg/Main.java:4:44
      |modB/lib/reference.conf:3:5
      |modB/lib/reference.conf:6:21
      |modB/lib/reference.conf:7:17
      |modB/libsrc/blibpkg/LibMain.java:4:44
      |modB/src/application.conf:1:11
      |modB/src/application.conf:2:20
      |modB/src/application.conf:3:17
      |modB/src/bpkg/Main.java:4:44
      |""".stripMargin

  def testHoconUsagesFromSources(): Unit =
    testFindUsages[HKey]("modA/src/application.conf", 1, 11, expectedHoconUsages)

  def testHoconUsagesFromLib(): Unit =
    testFindUsages[HKey]("modA/lib/reference.conf", 3, 5, expectedHoconUsages)

  def testHoconUsagesFromLibSources(): Unit =
    testFindUsages[HKey]("modA/libsrc/reference.conf", 3, 5, expectedHoconUsages)

  def testHoconUsagesFromDepModuleSources(): Unit =
    testFindUsages[HKey]("modB/src/application.conf", 1, 11, expectedHoconUsages)

  def testHoconUsagesFromDepModuleLib(): Unit =
    testFindUsages[HKey]("modB/lib/reference.conf", 3, 5, expectedHoconUsages)

  def testHoconUsagesFromDepModuleLibSources(): Unit =
    testFindUsages[HKey]("modB/libsrc/reference.conf", 3, 5, expectedHoconUsages)

  def testJavaClassUsages(): Unit =
    testFindUsages[PsiClass]("modA/src/pkg/Main.java", 3, 14,
      """modA/lib/reference.conf:8:13
        |modA/libsrc/reference.conf:8:13
        |modA/src/application.conf:4:13
        |""".stripMargin)

  private def testFindUsages[E >: Null <: PsiElement : ClassTag](
    filename: String, line: Int, column: Int, expected: String
  ): Unit = {
    val file = findFile(filename, fixture.getProject)
    fixture.openFileInEditor(file.getVirtualFile)
    val offset = fixture.getEditor.logicalPositionToOffset(new LogicalPosition(line - 1, column - 1))
    val hkey = file.findElementAt(offset).parentOfType[E].orNull
    val fixtureImpl = fixture.asInstanceOf[CodeInsightTestFixtureImpl] // findUsages with scope is not exposed...
    val usages = fixtureImpl.findUsages(hkey, ProjectScope.getAllScope(fixture.getProject))
    val usagesRepr = usages.asScala.toVector.map { ui =>
      val vfile = ui.getVirtualFile
      fixture.openFileInEditor(vfile)
      val logicalPosition = fixture.getEditor.offsetToLogicalPosition(ui.getNavigationOffset)
      val usageFilename = vfile.getCanonicalPath.stripPrefix(contentRoot.getCanonicalPath + "/")
      s"$usageFilename:${logicalPosition.line + 1}:${logicalPosition.column + 1}"
    }.sorted.distinct.mkString("", "\n", "\n")
    assertEquals(expected, usagesRepr)
  }
}

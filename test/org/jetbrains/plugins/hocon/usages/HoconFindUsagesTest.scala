package org.jetbrains.plugins.hocon
package usages

import com.intellij.openapi.editor.LogicalPosition
import com.intellij.psi.search.ProjectScope
import com.intellij.testFramework.fixtures.impl.CodeInsightTestFixtureImpl
import org.jetbrains.plugins.hocon.psi.HKey
import org.junit.Assert.assertEquals

class HoconFindUsagesTest extends HoconMultiModuleTest {
  def rootPath: String = "testdata/findUsages"

  override def moduleDependencies: Seq[(String, String)] = List("modA" -> "modB")

  private val expectedUsages =
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

  def testFindUsagesFromSources(): Unit =
    testFindUsages("modA/src/application.conf", 1, 11)

  def testFindUsagesFromLib(): Unit =
    testFindUsages("modA/lib/reference.conf", 3, 5)

  def testFindUsagesFromLibSources(): Unit =
    testFindUsages("modA/libsrc/reference.conf", 3, 5)

  def testFindUsagesFromDepModuleSources(): Unit =
    testFindUsages("modB/src/application.conf", 1, 11)

  def testFindUsagesFromDepModuleLib(): Unit =
    testFindUsages("modB/lib/reference.conf", 3, 5)

  def testFindUsagesFromDepModuleLibSources(): Unit =
    testFindUsages("modB/libsrc/reference.conf", 3, 5)

  private def testFindUsages(filename: String, line: Int, column: Int): Unit = {
    val file = findHoconFile(filename, fixture.getProject)
    fixture.openFileInEditor(file.getVirtualFile)
    val offset = fixture.getEditor.logicalPositionToOffset(new LogicalPosition(line - 1, column - 1))
    val hkey = file.findElementAt(offset).parentOfType[HKey].orNull
    val fixtureImpl = fixture.asInstanceOf[CodeInsightTestFixtureImpl] // findUsages with scope is not exposed...
    val usages = fixtureImpl.findUsages(hkey, ProjectScope.getAllScope(fixture.getProject))
    val usagesRepr = usages.asScala.toVector.map { ui =>
      val vfile = ui.getVirtualFile
      fixture.openFileInEditor(vfile)
      val logicalPosition = fixture.getEditor.offsetToLogicalPosition(ui.getNavigationOffset)
      val usageFilename = vfile.getCanonicalPath.stripPrefix(contentRoot.getCanonicalPath + "/")
      s"$usageFilename:${logicalPosition.line + 1}:${logicalPosition.column + 1}"
    }.sorted.distinct.mkString("", "\n", "\n")
    assertEquals(expectedUsages, usagesRepr)
  }
}

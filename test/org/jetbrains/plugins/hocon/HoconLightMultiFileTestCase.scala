package org.jetbrains.plugins.hocon

import com.intellij.openapi.roots.ModuleRootManager
import com.intellij.openapi.vfs.LocalFileSystem
import com.intellij.testFramework.LightPlatformTestCase.getModule
import com.intellij.testFramework.{LightPlatformCodeInsightTestCase, PsiTestUtil}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.util.TestUtils

/**
 * Base class for multi-file HOCON tests. Adds a specified testdata directory as module content root.
 */
abstract class HoconLightMultiFileTestCase extends LightPlatformCodeInsightTestCase {
  protected def rootPath: String

  protected def baseRootPath = TestUtils.getTestDataPath + "/hocon/"

  override def setUp() = {
    super.setUp()

    val rootManager: ModuleRootManager = ModuleRootManager.getInstance(getModule)
    val rootModel = rootManager.getModifiableModel
    val testDataRoot = LocalFileSystem.getInstance.refreshAndFindFileByPath(rootPath)
    assert(testDataRoot != null)
    val contentEntry = rootModel.addContentEntry(testDataRoot)
    contentEntry.addSourceFolder(testDataRoot, false)

    inWriteAction {
      rootModel.commit()
    }
  }

  override def tearDown() = {
    val testDataRoot = LocalFileSystem.getInstance.refreshAndFindFileByPath(rootPath)
    assert(testDataRoot != null)

    PsiTestUtil.removeContentEntry(getModule, testDataRoot)

    super.tearDown()
  }
}

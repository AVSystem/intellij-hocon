package org.jetbrains.plugins.hocon

import com.intellij.openapi.roots.ModuleRootManager
import com.intellij.openapi.vfs.{LocalFileSystem, VirtualFile}
import com.intellij.testFramework.LightPlatformCodeInsightTestCase
import com.intellij.testFramework.LightPlatformTestCase.getModule
import com.intellij.testFramework.PsiTestUtil.removeContentEntry
import org.junit.Assert.fail

abstract class HoconSingleModuleTest extends LightPlatformCodeInsightTestCase with HoconTestUtils {
  protected def rootPath: String

  protected final def contentRoot: Option[VirtualFile] = {
    val fileSystem = LocalFileSystem.getInstance()
    Option(fileSystem.findFileByPath(rootPath))
  }

  override def setUp(): Unit = {
    super.setUp()

    val rootModel = ModuleRootManager.getInstance(getModule).getModifiableModel
    val testDataRoot = contentRoot.getOrElse {
      fail()
      return
    }

    rootModel.addContentEntry(testDataRoot)
      .addSourceFolder(testDataRoot, false)

    inWriteAction {
      rootModel.commit()
    }
  }

  override def tearDown(): Unit = {
    contentRoot.foreach(removeContentEntry(getModule, _))
    super.tearDown()
  }
}

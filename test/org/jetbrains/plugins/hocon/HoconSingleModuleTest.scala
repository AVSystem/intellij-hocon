package org.jetbrains.plugins.hocon

import com.intellij.openapi.roots.ModuleRootManager
import com.intellij.testFramework.PsiTestUtil.removeContentEntry
import com.intellij.testFramework.{LightPlatformCodeInsightTestCase, LightPlatformTestCase}

abstract class HoconSingleModuleTest extends LightPlatformCodeInsightTestCase with HoconTestUtils {
  final def project = LightPlatformTestCase.getProject
  final def module = LightPlatformTestCase.getModule
  final def psiManager = LightPlatformTestCase.getPsiManager

  override def setUp(): Unit = {
    super.setUp()

    val rootModel = ModuleRootManager.getInstance(module).getModifiableModel
    rootModel.addContentEntry(contentRoot).addSourceFolder(contentRoot, false)

    inWriteAction {
      rootModel.commit()
    }
  }

  override def tearDown(): Unit = {
    removeContentEntry(module, contentRoot)
    super.tearDown()
  }
}

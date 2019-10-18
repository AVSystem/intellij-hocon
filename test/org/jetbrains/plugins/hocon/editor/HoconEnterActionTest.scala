package org.jetbrains.plugins.hocon
package editor

import com.intellij.openapi.actionSystem.IdeActions.ACTION_EDITOR_ENTER

/**
  * @author ghik
  */
class HoconEnterActionTest extends HoconEditorActionTest(ACTION_EDITOR_ENTER, "enter")
object HoconEnterActionTest extends TestSuiteCompanion[HoconEnterActionTest]

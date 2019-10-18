package org.jetbrains.plugins.hocon
package editor

import com.intellij.openapi.actionSystem.IdeActions.ACTION_EDITOR_JOIN_LINES

/**
  * @author ghik
  */
class HoconJoinLinesTest extends HoconEditorActionTest(ACTION_EDITOR_JOIN_LINES, "joinLines")
object HoconJoinLinesTest extends TestSuiteCompanion[HoconJoinLinesTest]

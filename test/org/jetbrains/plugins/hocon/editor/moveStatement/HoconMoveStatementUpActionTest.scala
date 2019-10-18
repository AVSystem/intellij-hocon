package org.jetbrains.plugins.hocon
package editor
package moveStatement

import com.intellij.openapi.actionSystem.IdeActions.ACTION_MOVE_STATEMENT_UP_ACTION

/**
  * @author ghik
  */
class HoconMoveStatementUpActionTest extends HoconEditorActionTest(ACTION_MOVE_STATEMENT_UP_ACTION, "moveStatement/up")
object HoconMoveStatementUpActionTest extends TestSuiteCompanion[HoconMoveStatementUpActionTest]

package org.jetbrains.plugins.hocon
package editor
package moveStatement

import com.intellij.openapi.actionSystem.IdeActions.ACTION_MOVE_STATEMENT_DOWN_ACTION

/**
  * @author ghik
  */
class HoconMoveStatementDownOnlyActionTest extends HoconEditorActionTest(ACTION_MOVE_STATEMENT_DOWN_ACTION, "moveStatement/down")
object HoconMoveStatementDownOnlyActionTest extends TestSuiteCompanion[HoconMoveStatementDownOnlyActionTest]

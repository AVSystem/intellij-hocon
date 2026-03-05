package org.jetbrains.plugins.hocon
package editor

import com.intellij.codeInsight.editorActions.enter.EnterBetweenBracesDelegate

class HoconEnterBetweenBracesHandler extends EnterBetweenBracesDelegate {
  override protected def isBracePair(c1: Char, c2: Char): Boolean =
    c1 == '{' && c2 == '}' || c1 == '[' && c2 == ']'
}

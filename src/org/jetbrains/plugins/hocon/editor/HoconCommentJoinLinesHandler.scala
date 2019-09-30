package org.jetbrains.plugins.hocon.editor

import com.intellij.codeInsight.editorActions.JoinLinesHandlerDelegate.CANNOT_JOIN
import com.intellij.codeInsight.editorActions.JoinRawLinesHandlerDelegate
import com.intellij.openapi.editor.Document
import com.intellij.psi.{PsiFile, PsiWhiteSpace}
import org.jetbrains.plugins.hocon.lexer.{HoconTokenSets, HoconTokenType}
import org.jetbrains.plugins.hocon.psi.HoconPsiFile

import scala.annotation.tailrec

/**
 * HOCON line comments can start with either '//' or '#'. Unfortunately, only one of them can be declared in
 * [[HoconCommenter]] and so I need this custom join lines handler to properly handle both.
 */
class HoconCommentJoinLinesHandler extends JoinRawLinesHandlerDelegate {
  def tryJoinRawLines(document: Document, file: PsiFile, start: Int, end: Int): Int = file match {
    case _: HoconPsiFile =>
      val line = document.getLineNumber(start)
      file.findElementAt(start) match {
        case ws: PsiWhiteSpace =>
          val prev = ws.getPrevSibling
          val next = ws.getNextSibling
          val prevType = prev.getNode.getElementType
          val nextType = next.getNode.getElementType

          if (document.getLineNumber(prev.getTextOffset) == line &&
            document.getLineNumber(next.getTextOffset) == line + 1 &&
            HoconTokenSets.Comment.contains(prevType) &&
            HoconTokenSets.Comment.contains(nextType)) {

            val charSeq = document.getCharsSequence
            @tailrec def nextLineNewStart(idx: Int): Int =
              if (idx >= charSeq.length) idx
              else if (charSeq.charAt(idx).isWhitespace) nextLineNewStart(idx + 1)
              else idx

            val firstIdx = nextType match {
              case HoconTokenType.HashComment => end + 1
              case HoconTokenType.DoubleSlashComment => end + 2
            }
            document.replaceString(start, nextLineNewStart(firstIdx), " ")
            start
          }
          else CANNOT_JOIN
        case _ =>
          CANNOT_JOIN
      }
    case _ => CANNOT_JOIN
  }

  def tryJoinLines(document: Document, file: PsiFile, start: Int, end: Int): Int =
    CANNOT_JOIN
}

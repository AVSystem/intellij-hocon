package org.jetbrains.plugins.hocon
package includes

import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.hocon.lexer.HoconTokenType
import org.jetbrains.plugins.hocon.psi.{HIncludeTarget, HoconPsiFile}
import org.jetbrains.plugins.hocon.ref.IncludedFileReference
import org.junit.Assert.{assertEquals, assertTrue}

/**
 * @author ghik
 */
trait HoconIncludeResolutionTest extends HoconTestUtils {
  protected def checkFile(path: String, project: Project): Unit = {
    val psiFile = findHoconFile(path, project)

    new HoconIncludeResolutionTest.DepthFirstIterator(psiFile).collect {
      case target: HIncludeTarget => target
    }.foreach { target =>
      val parent = target.parent
      val prevComments = parent.parent.nonWhitespaceChildren
        .takeWhile(e => e.getNode.getElementType == HoconTokenType.HashComment)
        .toVector

      val references = target.getFileReferences

      if (prevComments.nonEmpty) {
        assertTrue("No references in " + parent.getText, references.nonEmpty)
        val resolveResults = references.last.multiResolve(false)
        resolveResults.sliding(2).foreach {
          case Array(rr1, rr2) =>
            assertTrue(IncludedFileReference.ResolveResultOrdering.lteq(rr1, rr2))
          case _ =>
        }

        val expectedFiles = prevComments.flatMap(_.getText.stripPrefix("#").split(','))
          .map(_.trim)
          .filter(_.nonEmpty)
          .flatMap(path => findVirtualFile(path).opt)

        val actualFiles = resolveResults
          .map(_.getElement)
          .collect {
            case file: PsiFile => file.getVirtualFile
          }

        assertEquals(parent.getText, expectedFiles.toSet, actualFiles.toSet)
      } else {
        assertTrue("Expected no references in " + parent.getText, references.isEmpty)
      }
    }
  }

}

object HoconIncludeResolutionTest {
  private class DepthFirstIterator(file: HoconPsiFile) extends Iterator[PsiElement] {
    private var stack: List[PsiElement] = Nil

    def hasNext: Boolean = stack.nonEmpty

    def next(): PsiElement = {
      val head :: tail = stack
      stack = tail

      var child = head.getLastChild
      while (child != null) {
        stack = child :: stack
        child = child.getPrevSibling
      }

      head
    }
  }
}
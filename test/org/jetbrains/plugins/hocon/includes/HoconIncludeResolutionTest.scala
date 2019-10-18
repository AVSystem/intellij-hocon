package org.jetbrains.plugins.hocon
package includes

import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile
import org.jetbrains.plugins.hocon.lexer.HoconTokenType
import org.jetbrains.plugins.hocon.psi.HIncludeTarget
import org.jetbrains.plugins.hocon.ref.IncludedFileReference
import org.junit.Assert.{assertEquals, assertTrue}

/**
 * @author ghik
 */
trait HoconIncludeResolutionTest extends HoconTestUtils {
  protected def checkFile(path: String, project: Project): Unit = {
    val psiFile = findHoconFile(path, project)

    psiFile.depthFirst.collectOnly[HIncludeTarget].foreach { target =>
      val hincluded = target.parent.parent
      val prevComments = hincluded.parent.nonWhitespaceChildren
        .takeWhile(e => e.getNode.getElementType == HoconTokenType.HashComment)
        .toVector

      val references = target.getFileReferences

      if (prevComments.nonEmpty) {
        assertTrue("No references in " + hincluded.getText, references.nonEmpty)
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

        assertEquals(hincluded.getText, expectedFiles.toSet, actualFiles.toSet)
      } else {
        assertTrue("Expected no references in " + hincluded.getText, references.isEmpty)
      }
    }
  }

}

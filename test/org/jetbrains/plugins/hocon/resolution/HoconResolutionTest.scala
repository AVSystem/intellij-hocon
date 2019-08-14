package org.jetbrains.plugins.hocon
package resolution

import org.jetbrains.plugins.hocon.psi._
import org.junit.Assert

class HoconResolutionTest extends HoconSingleModuleTest {
  def rootPath = s"$testdataPath/hocon/resolution"

  private def render(occurences: Iterator[ResolvedField]): String =
    occurences.map(rf => s"${rf.field.hoconFile.getName}:${rf.field.pos}").mkString("\n")

  private def testPath(file: String, hoconPath: String, expected: String): Unit = {
    val expectedSplit = expected.split("\n").iterator.map(_.trim).filter(_.nonEmpty).toList
    val hoconFile = findHoconFile(file, project)
    val ctx = ToplevelCtx(hoconFile, ToplevelCtx.referenceFilesFor(hoconFile))
    val path = hoconPath.split('.').toList

    def test(reverse: Boolean): Unit = {
      val opts = ResOpts(reverse)
      val expectedStr = (if (reverse) expectedSplit.reverse else expectedSplit).mkString("\n")

      val actualResult = render(ctx.occurrences(path, opts))
      Assert.assertEquals(expectedStr, actualResult)

      val traversalResult = render(Iterator.iterate(ctx.occurrences(path, opts).nextOption.orNull) { rf =>
        rf.nextOccurrence(opts).orNull
      }.takeWhile(_ != null))
      Assert.assertEquals(expectedStr, traversalResult)
    }

    test(reverse = true)
    test(reverse = false)
  }

  def testSimpleResolution(): Unit = testPath("application.conf", "a.b.c",
    """reference.conf:1:4
      |application.conf:1:4
      |application.conf:4:4
      |application.conf:5:7
      |application.conf:5:15
      |application.conf:13:2
      |application.conf:9:2
      |included.conf:1:4
      |moreIncluded.conf:1:4
      |included.conf:3:4
      |nestedIncluded.conf:1:2
      |application.conf:21:4
      |application.conf:24:4
      |""".stripMargin
  )
}

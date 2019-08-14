package org.jetbrains.plugins.hocon
package resolution

import org.jetbrains.plugins.hocon.psi._
import org.junit.Assert

class HoconResolutionTest extends HoconSingleModuleTest {
  def rootPath = s"$testdataPath/hocon/resolution"

  private def render(occurences: Iterator[ResolvedField]): String =
    occurences.map(rf => s"${rf.field.hoconFile.getName}:${rf.field.pos}").mkString("", "\n", "\n")

  private def testPath(file: String, hoconPath: String, expected: String): Unit = {
    val hoconFile = findHoconFile(file, project)
    val ctx = ToplevelCtx(hoconFile, ToplevelCtx.referenceFilesFor(hoconFile))
    val path = hoconPath.split('.').toList
    val actualResult = render(ctx.occurrences(path, reverse = true))
    Assert.assertEquals(expected, actualResult)
    val traversalResult = render(Iterator.iterate(ctx.occurrences(path, reverse = true).nextOption.orNull) { rf =>
      rf.nextOccurrence(reverse = true).orNull
    }.takeWhile(_ != null))
    Assert.assertEquals(expected, traversalResult)
  }

  def testSimpleResolution(): Unit = testPath("application.conf", "a.b.c",
    """application.conf:22:4
      |nestedIncluded.conf:1:2
      |included.conf:3:4
      |moreIncluded.conf:1:4
      |included.conf:1:4
      |application.conf:9:2
      |application.conf:13:2
      |application.conf:5:15
      |application.conf:5:7
      |application.conf:4:4
      |application.conf:1:4
      |reference.conf:1:4
      |""".stripMargin
  )
}

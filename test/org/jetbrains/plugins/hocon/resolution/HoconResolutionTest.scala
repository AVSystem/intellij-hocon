package org.jetbrains.plugins.hocon
package resolution

import org.jetbrains.plugins.hocon.psi._
import org.junit.Assert

class HoconResolutionTest extends HoconSingleModuleTest {
  def rootPath = s"$testdataPath/hocon/resolution"

  private def testPath(file: String, hoconPath: String, expected: String): Unit = {
    val hoconFile = findHoconFile(file, project)
    val ctx = ToplevelCtx(hoconFile, ToplevelCtx.referenceFilesFor(hoconFile))
    val path = hoconPath.split('.').toList

    def test(reverse: Boolean): Unit = {
      def render(occurences: Iterator[ResolvedField]): String = {
        val rendered = occurences.map(rf => s"${rf.field.hoconFile.getName}:${rf.field.pos}")
        (if (reverse) rendered.toList.reverseIterator else rendered).mkString("", "\n", "\n")
      }

      val opts = ResOpts(reverse)
      val actualResult = render(ctx.occurrences(path, opts))
      Assert.assertEquals(expected, actualResult)

      val traversalResult = render(Iterator.iterate(ctx.occurrences(path, opts).nextOption.orNull) { rf =>
        rf.nextOccurrence(opts).orNull
      }.takeWhile(_ != null))
      Assert.assertEquals(expected, traversalResult)
    }

    test(reverse = true)
    test(reverse = false)
  }

  def testSingleKey(): Unit = testPath("application.conf", "a",
    """reference.conf:1:0
      |application.conf:1:0
      |application.conf:3:0
      |application.conf:8:0
      |included.conf:1:0
      |moreIncluded.conf:1:0
      |included.conf:3:0
      |application.conf:18:0
      |application.conf:21:0
      |application.conf:27:0
      |""".stripMargin
  )

  def testMidPath(): Unit = testPath("application.conf", "a.b",
    """reference.conf:1:2
      |application.conf:1:2
      |application.conf:4:2
      |application.conf:5:2
      |application.conf:8:2
      |included.conf:1:2
      |moreIncluded.conf:1:2
      |included.conf:3:2
      |nestedIncluded.conf:1:0
      |nestedIncluded.conf:2:0
      |nestedIncluded.conf:3:0
      |application.conf:24:2
      |application.conf:27:2
      |""".stripMargin
  )

  def testFullPath(): Unit = testPath("application.conf", "a.b.c",
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
      |application.conf:18:9
      |application.conf:19:16
      |application.conf:24:4
      |application.conf:27:4
      |""".stripMargin
  )
}

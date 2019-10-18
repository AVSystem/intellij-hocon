package org.jetbrains.plugins.hocon
package resolution

import org.jetbrains.plugins.hocon.semantics.{ResOpts, ResolvedField, ToplevelCtx}
import org.junit.Assert

class HoconResolutionTest extends HoconSingleModuleTest {
  def rootPath = s"$testdataPath/resolution"

  private def testPath(file: String, hoconPath: String, expected: String): Unit = {
    val hoconFile = findHoconFile(file, project)
    val ctx = ToplevelCtx(hoconFile)
    val path = hoconPath.split('.').toList

    def test(reverse: Boolean): Unit = {
      def render(occurences: Iterator[ResolvedField]): String = {
        val rendered = occurences.map(_.trace)
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
      |application.conf:2:0
      |application.conf:5:0
      |application.conf:7:0
      |application.conf:12:0
      |application.conf:20:0->included.conf:1:0
      |application.conf:20:0->included.conf:2:0->moreIncluded.conf:1:0
      |application.conf:20:0->included.conf:3:0
      |application.conf:22:0
      |application.conf:25:0
      |application.conf:31:0
      |application.conf:33:0
      |""".stripMargin
  )

  def testMidPath(): Unit = testPath("application.conf", "a.b",
    """reference.conf:1:2
      |application.conf:1:2
      |application.conf:2:2
      |application.conf:5:2
      |application.conf:8:2
      |application.conf:9:2
      |application.conf:12:2
      |application.conf:20:0->included.conf:1:2
      |application.conf:20:0->included.conf:2:0->moreIncluded.conf:1:2
      |application.conf:20:0->included.conf:3:2
      |application.conf:26:2->nestedIncluded.conf:1:0
      |application.conf:26:2->nestedIncluded.conf:2:0
      |application.conf:26:2->nestedIncluded.conf:3:0
      |application.conf:28:2
      |application.conf:31:2
      |application.conf:33:2
      |""".stripMargin
  )

  def testFullPath(): Unit = testPath("application.conf", "a.b.c",
    // some entries duplicated due to self-referential substitutions
    """reference.conf:1:4
      |application.conf:1:4
      |application.conf:5:7
      |reference.conf:1:4
      |application.conf:1:4
      |application.conf:2:6
      |application.conf:8:4
      |application.conf:9:7
      |application.conf:9:15
      |application.conf:17:2
      |application.conf:13:2
      |application.conf:20:0->included.conf:1:4
      |application.conf:20:0->included.conf:2:0->moreIncluded.conf:1:4
      |application.conf:20:0->included.conf:3:4
      |application.conf:26:2->nestedIncluded.conf:1:2
      |application.conf:22:9
      |application.conf:23:16
      |application.conf:28:4
      |application.conf:31:4
      |""".stripMargin
  )
}

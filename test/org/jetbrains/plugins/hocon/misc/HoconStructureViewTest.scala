package org.jetbrains.plugins.hocon
package misc

import com.intellij.ide.util.treeView.smartTree.TreeElement
import com.intellij.testFramework.LightPlatformCodeInsightTestCase
import org.jetbrains.plugins.hocon.psi.HoconPsiFile
import org.junit.Assert.assertEquals

class HoconStructureViewTest extends LightPlatformCodeInsightTestCase {

  private def render(text: String): String = {
    configureFromFileText("test.conf", text)
    val file = getFile.asInstanceOf[HoconPsiFile]
    val model = new HoconStructureViewModel(file, getEditor)
    try {
      val sb = new StringBuilder
      def walk(el: TreeElement, depth: Int): Unit = {
        val p = el.getPresentation
        val loc = Option(p.getLocationString).filter(_.nonEmpty).map(s => s" = $s").getOrElse("")
        sb.append("  " * depth).append(p.getPresentableText).append(loc).append('\n')
        el.getChildren.foreach(walk(_, depth + 1))
      }
      model.getRoot.getChildren.foreach(walk(_, 0))
      sb.toString
    } finally model.dispose()
  }

  def testTree(): Unit = {
    val conf =
      """server {
        |  host = localhost
        |  port = 8080
        |}
        |db.url = "jdbc:postgresql://localhost/app"
        |list = [1, 2, 3]
        |a.b.c {
        |  d = true
        |}
        |include "common.conf"
        |""".stripMargin
    // - objects are expanded, leaf values show a value preview
    // - prefixed paths (a.b.c) collapse to a single node
    // - arrays show a compact placeholder, includes are listed as leaves
    val expected =
      """server
        |  host = localhost
        |  port = 8080
        |db.url = "jdbc:postgresql://localhost/app"
        |list = […]
        |a.b.c
        |  d = true
        |include common.conf
        |""".stripMargin
    assertEquals(expected, render(conf))
  }
}

package org.jetbrains.plugins.hocon
package includes

class HoconSingleModuleIncludeResolutionTest extends HoconSingleModuleTest with HoconIncludeResolutionTest {
  def rootPath = s"$testdataPath/includes/singlemodule"

  def testIncludesFromTopLevel(): Unit =
    checkFile("including.conf")

  def testIncludesFromWithinPackage(): Unit =
    checkFile("pkg/including.conf")

  private def checkFile(path: String): Unit =
    checkFile(path, getProject)
}

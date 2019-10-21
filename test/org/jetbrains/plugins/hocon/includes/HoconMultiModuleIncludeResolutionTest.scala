package org.jetbrains.plugins.hocon
package includes

/**
 * @author ghik
 */
class HoconMultiModuleIncludeResolutionTest extends HoconMultiModuleTest with HoconIncludeResolutionTest {

  def rootPath = "testdata/includes/multimodule"

  override def moduleDependencies: Seq[(String, String)] =
    List("modA" -> "modB", "modB" -> "modC")

  def testIncludeFromLibrary(): Unit =
    checkFile("modC/src/including.conf")

  def testIncludeFromModuleDependency(): Unit =
    checkFile("modB/src/including.conf")

  def testIncludeFromTransitiveModuleDependency(): Unit =
    checkFile("modA/src/including.conf")

  def testIncludeInLibrary(): Unit =
    checkFile("modC/lib/including.conf")

  def testIncludeInLibraryFromModuleDependency(): Unit =
    checkFile("modB/lib/including.conf")

  def testIncludeInLibraryFromTransitiveModuleDependency(): Unit =
    checkFile("modA/lib/including.conf")

  def testIncludeInLibrarySources(): Unit =
    checkFile("modC/libsrc/including.conf")

  def testIncludeInLibrarySourcesFromModuleDependency(): Unit =
    checkFile("modB/libsrc/including.conf")

  def testIncludeInLibrarySourcesFromTransitiveModuleDependency(): Unit =
    checkFile("modA/libsrc/including.conf")

  def testIncludeInTestsFromLibrary(): Unit =
    checkFile("modC/testsrc/including.conf")

  def testIncludeInTestsFromModuleDependency(): Unit =
    checkFile("modB/testsrc/including.conf")

  def testIncludeInTestsFromTransitiveModuleDependency(): Unit =
    checkFile("modA/testsrc/including.conf")

  def testIncludeInTestLibrary(): Unit =
    checkFile("modC/testlib/including.conf")

  def testIncludeInTestLibraryFromModuleDependency(): Unit =
    checkFile("modB/testlib/including.conf")

  def testIncludeInTestLibraryFromTransitiveModuleDependency(): Unit =
    checkFile("modA/testlib/including.conf")

  def testIncludeInTestLibrarySources(): Unit =
    checkFile("modC/testlibsrc/including.conf")

  def testIncludeInTestLibrarySourcesFromModuleDependency(): Unit =
    checkFile("modB/testlibsrc/including.conf")

  def testIncludeInTestLibrarySourcesFromTransitiveModuleDependency(): Unit =
    checkFile("modA/testlibsrc/including.conf")

  def testIncludeFromNonSourceDirectory(): Unit =
    checkFile("modC/other/including.conf")

  private def checkFile(path: String): Unit =
    checkFile(path, fixture.getProject)
}
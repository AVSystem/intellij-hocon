package org.jetbrains.plugins.hocon.includes

import java.io.File

import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.{DependencyScope, LibraryOrderEntry, ModuleRootManager}
import com.intellij.openapi.vfs.{LocalFileSystem, VirtualFile}
import com.intellij.testFramework.UsefulTestCase
import com.intellij.testFramework.builders.JavaModuleFixtureBuilder
import com.intellij.testFramework.fixtures._
import org.jetbrains.jps.model.java.JavaSourceRootType
import org.jetbrains.plugins.scala.extensions._

/**
 * @author ghik
 */
class HoconMultiModuleIncludeResolutionTest extends UsefulTestCase with HoconIncludeResolutionTest {
  private var fixture: CodeInsightTestFixture = null
  private var modules: Map[String, Module] = null
  private val testdataPath = "testdata/hocon/includes/multimodule"

  protected def project: Project = fixture.getProject

  protected def contentRoots: Array[VirtualFile] =
    Array(LocalFileSystem.getInstance.findFileByPath(testdataPath))

  override def setUp(): Unit = {
    super.setUp()

    val fixtureBuilder = IdeaTestFixtureFactory.getFixtureFactory.createFixtureBuilder(getName)
    fixture = JavaTestFixtureFactory.getFixtureFactory.createCodeInsightFixture(fixtureBuilder.getFixture)

    val baseDir = new File(testdataPath)
    val moduleDirs = baseDir.listFiles.sortBy(_.getName).iterator.filter(_.isDirectory)
    val moduleFixtures = moduleDirs.map { dir =>
      val builder = fixtureBuilder.addModule(classOf[JavaModuleFixtureBuilder[ModuleFixture]])
      builder.addContentRoot(dir.getPath)
      builder.addLibrary(dir.getName + "lib", new File(dir, "lib").getPath)
      builder.addLibrary(dir.getName + "testlib", new File(dir, "testlib").getPath)
      (dir.getName, builder.getFixture)
    }.toMap

    fixture.setUp()
    fixture.setTestDataPath(testdataPath)

    modules = moduleFixtures.mapValues(_.getModule)

    inWriteAction {
      LocalFileSystem.getInstance().refresh(false)

      modules.values.foreach { mod =>
        val model = ModuleRootManager.getInstance(mod).getModifiableModel
        val contentEntry = model.getContentEntries.head
        contentEntry.addSourceFolder(contentEntry.getFile.findChild("src"), JavaSourceRootType.SOURCE)
        contentEntry.addSourceFolder(contentEntry.getFile.findChild("testsrc"), JavaSourceRootType.TEST_SOURCE)
        model.getOrderEntries.foreach {
          case loe: LibraryOrderEntry if loe.getLibraryName.endsWith("testlib") =>
            loe.setScope(DependencyScope.TEST)
          case _ =>
        }
        model.commit()
      }

      def addDependency(dependingModule: Module, dependencyModule: Module): Unit = {
        val model = ModuleRootManager.getInstance(dependingModule).getModifiableModel
        model.addModuleOrderEntry(dependencyModule).setExported(true)
        model.commit()
      }

      addDependency(modules("modA"), modules("modB"))
      addDependency(modules("modB"), modules("modC"))
    }
  }

  override def tearDown(): Unit = {
    modules = null
    fixture.tearDown()
    fixture = null
    super.tearDown()
  }

  def testIncludeFromLibrary(): Unit = {
    checkFile("modC/src/including.conf")
  }

  def testIncludeFromModuleDependency(): Unit = {
    checkFile("modB/src/including.conf")
  }

  def testIncludeFromTransitiveModuleDependency(): Unit = {
    checkFile("modA/src/including.conf")
  }

  def testIncludeInLibrary(): Unit = {
    checkFile("modC/lib/including.conf")
  }

  def testIncludeInLibraryFromModuleDependency(): Unit = {
    checkFile("modB/lib/including.conf")
  }

  def testIncludeInLibraryFromTransitiveModuleDependency(): Unit = {
    checkFile("modA/lib/including.conf")
  }

  def testIncludeInTestsFromLibrary(): Unit = {
    checkFile("modC/testsrc/including.conf")
  }

  def testIncludeInTestsFromModuleDependency(): Unit = {
    checkFile("modB/testsrc/including.conf")
  }

  def testIncludeInTestsFromTransitiveModuleDependency(): Unit = {
    checkFile("modA/testsrc/including.conf")
  }

  def testIncludeInTestLibrary(): Unit = {
    checkFile("modC/testlib/including.conf")
  }

  def testIncludeInTestLibraryFromModuleDependency(): Unit = {
    checkFile("modB/testlib/including.conf")
  }

  def testIncludeInTestLibraryFromTransitiveModuleDependency(): Unit = {
    checkFile("modA/testlib/including.conf")
  }
}

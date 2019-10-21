package org.jetbrains.plugins.hocon

import java.io.File

import com.intellij.openapi.module.Module
import com.intellij.openapi.roots._
import com.intellij.openapi.vfs.LocalFileSystem
import com.intellij.testFramework.UsefulTestCase
import com.intellij.testFramework.builders.JavaModuleFixtureBuilder
import com.intellij.testFramework.fixtures._
import com.intellij.testFramework.fixtures.impl.{JavaModuleFixtureBuilderImpl, ModuleFixtureImpl}
import org.jetbrains.jps.model.java.JavaSourceRootType

abstract class HoconMultiModuleTest extends UsefulTestCase with HoconTestUtils {

  import HoconMultiModuleTest._

  private var _fixture: CodeInsightTestFixture = _

  protected def fixture: CodeInsightTestFixture = _fixture

  def moduleDependencies: Seq[(String, String)] = Seq.empty

  override def setUp(): Unit = {
    super.setUp()

    val testFixtureFactory = IdeaTestFixtureFactory.getFixtureFactory
    testFixtureFactory.registerFixtureBuilder(
      classOf[JavaModuleFixtureBuilder[ModuleFixture]], classOf[HoconJavaModuleFixtureBuilder])
    val fixtureBuilder = testFixtureFactory.createFixtureBuilder(getName)

    val moduleBuilders = subdirectories(rootPath)
      .map((_, fixtureBuilder.addModule(classOf[JavaModuleFixtureBuilder[ModuleFixture]])))

    moduleBuilders.foreach {
      case (directory, builder) =>
        builder.addContentRoot(directory.getPath)

        def addLibrary(libraryName: String): Unit = {
          import OrderRootType._
          val mapping = Map(CLASSES -> "", SOURCES -> "src").mapValues { suffix =>
            Array(new File(directory, libraryName + suffix).getPath)
          }

          builder.addLibrary(directory.getName + libraryName, mapping.asJava)
        }

        addLibrary("lib")
        addLibrary("testlib")
    }

    _fixture = JavaTestFixtureFactory.getFixtureFactory.createCodeInsightFixture(fixtureBuilder.getFixture)
    _fixture.setUp()
    _fixture.setTestDataPath(rootPath)

    inWriteAction {
      LocalFileSystem.getInstance().refresh(false)

      val modules = moduleBuilders.map {
        case (directory, builder) => (directory.getName, builder.getFixture.getModule)
      }.toMap

      val models = modules.mapValues { module =>
        ModuleRootManager.getInstance(module).getModifiableModel
      }
      models.values.foreach(setUpEntries)

      moduleDependencies.foreach {
        case (dependent, dependency) =>
          addDependency(models(dependent), modules(dependency))
      }
    }
  }

  override def tearDown(): Unit = {
    _fixture.tearDown()
    _fixture = null
    super.tearDown()
  }
}
object HoconMultiModuleTest {

  private def subdirectories(path: String): Seq[File] =
    new File(path).listFiles
      .filter(_.isDirectory)
      .sortBy(_.getName)

  private def setUpEntries(model: ModifiableRootModel): Unit = {
    val contentEntry = model.getContentEntries.head

    def addSourceFolder(name: String, kind: JavaSourceRootType): Unit = {
      contentEntry.addSourceFolder(contentEntry.getFile.findChild(name), kind)
    }

    import JavaSourceRootType._
    addSourceFolder("src", SOURCE)
    addSourceFolder("testsrc", TEST_SOURCE)

    model.getOrderEntries.collect {
      case entry: LibraryOrderEntry if entry.getLibraryName.endsWith("testlib") => entry
    }.foreach(_.setScope(DependencyScope.TEST))

    model.commit()
  }

  private def addDependency(model: ModifiableRootModel, module: Module): Unit = {
    model.addModuleOrderEntry(module).setExported(true)
    model.commit()
  }
}

class HoconJavaModuleFixtureBuilder(fixtureBuilder: TestFixtureBuilder[_ <: IdeaProjectTestFixture])
  extends JavaModuleFixtureBuilderImpl[ModuleFixture](fixtureBuilder) {

  def instantiateFixture(): ModuleFixture = new ModuleFixtureImpl(this)
}
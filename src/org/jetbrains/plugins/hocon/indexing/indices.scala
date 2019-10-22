package org.jetbrains.plugins.hocon
package indexing

import java.io.{DataInput, DataOutput}
import java.util.{Collections, Comparator}

import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ProjectFileIndex
import com.intellij.openapi.util.TextRange
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiManager
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.util.indexing.FileBasedIndex.{InputFilter, ValueProcessor}
import com.intellij.util.indexing._
import com.intellij.util.io.{DataExternalizer, EnumeratorStringDescriptor, KeyDescriptor}
import org.jetbrains.plugins.hocon.lang.HoconFileType
import org.jetbrains.plugins.hocon.psi.{HFieldKey, HKey, HSubstitutionKey, HoconPsiFile}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

case class HKeyOccurrences(
  inFields: ArrayBuffer[TextRange],
  inSubstitutions: ArrayBuffer[TextRange]
) {
  def all: Iterator[TextRange] = inFields.iterator ++ inSubstitutions.iterator
}
object HKeyOccurrences {
  def mkEmpty: HKeyOccurrences = HKeyOccurrences(new ArrayBuffer, new ArrayBuffer)

  object Externalizer extends DataExternalizer[HKeyOccurrences] {
    private def saveRanges(out: DataOutput, value: ArrayBuffer[TextRange]): Unit = {
      out.writeInt(value.length)
      value.foreach { range =>
        out.writeInt(range.getStartOffset)
        out.writeInt(range.getEndOffset)
      }
    }

    def save(dataOutput: DataOutput, t: HKeyOccurrences): Unit = {
      saveRanges(dataOutput, t.inFields)
      saveRanges(dataOutput, t.inSubstitutions)
    }

    private def readRanges(in: DataInput): ArrayBuffer[TextRange] = {
      val lb = new ArrayBuffer[TextRange]
      var len = in.readInt()
      lb.sizeHint(len)
      while (len > 0) {
        val startOffset = in.readInt()
        val endOffset = in.readInt()
        lb += new TextRange(startOffset, endOffset)
        len -= 1
      }
      lb.result()
    }

    def read(dataInput: DataInput): HKeyOccurrences = {
      val inFields = readRanges(dataInput)
      val inSubstitutions = readRanges(dataInput)
      HKeyOccurrences(inFields, inSubstitutions)
    }
  }
}

object TextRangeExternalizer extends DataExternalizer[TextRange] {
  def save(out: DataOutput, value: TextRange): Unit = {
    out.writeInt(value.getStartOffset)
    out.writeInt(value.getEndOffset)
  }
  def read(in: DataInput): TextRange =
    new TextRange(in.readInt(), in.readInt())
}

abstract class HKeyIndexCompanion[K](name: String) {
  final val Id: ID[K, HKeyOccurrences] = ID.create(name)

  def Version: Int
  def KeyDescriptor: KeyDescriptor[K]

  def processHKeys(
    dataKey: K,
    project: Project,
    filter: GlobalSearchScope = GlobalSearchScope.EMPTY_SCOPE,
    select: HKeyOccurrences => Iterator[TextRange] = occ => occ.inFields.iterator ++ occ.inSubstitutions.iterator
  )(processor: HKey => Boolean): Boolean = {
    val manager = PsiManager.getInstance(project)
    val pfi = ProjectFileIndex.getInstance(project)
    val indexProcessor: ValueProcessor[HKeyOccurrences] = (file, occurrences) => {
      val allFoundKeys = for {
        f <- file.opt.filterNot(pfi.isInLibrarySource).iterator
        hoconFile <- manager.findFile(f).opt.collectOnly[HoconPsiFile].iterator
        range <- select(occurrences)
        foundKey <- hoconFile.findElementAt(range.getStartOffset).parentOfType[HKey].iterator
      } yield foundKey
      allFoundKeys.forall(processor)
    }
    FileBasedIndex.getInstance.processValues(Id, dataKey, null, indexProcessor, filter)
  }
}

abstract class HKeyIndex[K](companion: HKeyIndexCompanion[K])
  extends FileBasedIndexExtension[K, HKeyOccurrences]
    with DataIndexer[K, HKeyOccurrences, FileContent]
    with InputFilter {

  def indexKey(hkey: HKey): Option[K]

  def getName: ID[K, HKeyOccurrences] = companion.Id
  def dependsOnFileContent(): Boolean = true
  def getVersion: Int = companion.Version

  def getInputFilter: InputFilter = this

  def getIndexer: DataIndexer[K, HKeyOccurrences, FileContent] = this

  def getKeyDescriptor: KeyDescriptor[K] = companion.KeyDescriptor
  def getValueExternalizer: DataExternalizer[HKeyOccurrences] = HKeyOccurrences.Externalizer

  def acceptInput(file: VirtualFile): Boolean =
    HoconFileType.isHocon(file.getFileType)

  def map(inputData: FileContent): JMap[K, HKeyOccurrences] = inputData.getPsiFile match {
    case hf: HoconPsiFile =>
      val result = new mutable.HashMap[K, HKeyOccurrences]

      for {
        hkey <- hf.depthFirst.collectOnly[HKey]
        ikey <- indexKey(hkey)
      } {
        val occurrences = result.getOrElseUpdate(ikey, HKeyOccurrences.mkEmpty)
        val buf = hkey match {
          case _: HFieldKey => occurrences.inSubstitutions
          case _: HSubstitutionKey => occurrences.inFields
        }
        buf += hkey.getTextRange
      }

      val rangeComparator: Comparator[TextRange] =
        (r1: TextRange, r2: TextRange) => Integer.compare(r1.getStartOffset, r2.getStartOffset)

      result.values.foreach { o =>
        Collections.sort(o.inFields.asJava, rangeComparator)
        Collections.sort(o.inSubstitutions.asJava, rangeComparator)
      }
      result.asJava
    case _ =>
      Collections.emptyMap[K, HKeyOccurrences]
  }
}

object HoconPathIndex extends HKeyIndexCompanion[List[String]]("hocon.paths") {
  final val Version = 0

  object KeyDescriptor extends DataExternalizer[List[String]]
    with KeyDescriptor[List[String]] {

    def getHashCode(value: List[String]): Int = value.hashCode
    def isEqual(val1: List[String], val2: List[String]): Boolean = val1 == val2

    def save(out: DataOutput, value: List[String]): Unit = {
      out.writeInt(value.length)
      value.foreach(EnumeratorStringDescriptor.INSTANCE.save(out, _))
    }

    def read(in: DataInput): List[String] = {
      val b = new ListBuffer[String]
      var len = in.readInt()
      b.sizeHint(len)
      while (len > 0) {
        b += EnumeratorStringDescriptor.INSTANCE.read(in)
        len -= 1
      }
      b.result()
    }
  }
}

class HoconPathIndex extends HKeyIndex[List[String]](HoconPathIndex) {
  def indexKey(hkey: HKey): Option[List[String]] =
    hkey.opt.filterNot(_.inFieldInArray).flatMap(_.fullStringPath)
}

object HoconKeyIndex extends HKeyIndexCompanion[String]("hocon.keys") {
  final val Version = 0
  final def KeyDescriptor: KeyDescriptor[String] = EnumeratorStringDescriptor.INSTANCE
}

class HoconKeyIndex extends HKeyIndex[String](HoconKeyIndex) {
  def indexKey(hkey: HKey): Option[String] = Some(hkey.stringValue)
}

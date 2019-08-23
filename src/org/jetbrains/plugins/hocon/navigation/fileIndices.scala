package org.jetbrains.plugins.hocon
package navigation

import java.io.{DataInput, DataOutput}
import java.util.Collections

import com.intellij.openapi.util.TextRange
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.util.indexing.FileBasedIndex.InputFilter
import com.intellij.util.indexing._
import com.intellij.util.io.{DataExternalizer, EnumeratorStringDescriptor, KeyDescriptor}
import org.jetbrains.plugins.hocon.lang.HoconFileType
import org.jetbrains.plugins.hocon.psi.{HKey, HoconPsiFile}

import scala.collection.mutable.ListBuffer

class ListExternalizer[T](elemExternalizer: DataExternalizer[T]) extends DataExternalizer[List[T]] {
  def save(out: DataOutput, value: List[T]): Unit = {
    out.writeInt(value.length)
    value.foreach(elemExternalizer.save(out, _))
  }

  def read(in: DataInput): List[T] = {
    val lb = new ListBuffer[T]
    var len = in.readInt()
    lb.sizeHint(len)
    while (len > 0) {
      lb += elemExternalizer.read(in)
      len -= 1
    }
    lb.result()
  }
}

object IntExternalizer extends DataExternalizer[Int] {
  def save(out: DataOutput, value: Int): Unit = out.writeInt(value)
  def read(in: DataInput): Int = in.readInt()
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
  final val Id: ID[K, List[TextRange]] = ID.create(name)

  def Version: Int
  def KeyDescriptor: KeyDescriptor[K]
}

abstract class HKeyIndex[K](companion: HKeyIndexCompanion[K])
  extends FileBasedIndexExtension[K, List[TextRange]]
    with DataIndexer[K, List[TextRange], FileContent]
    with InputFilter {

  def indexKey(hkey: HKey): Option[K]

  def getName: ID[K, List[TextRange]] = companion.Id
  def dependsOnFileContent(): Boolean = true
  def getVersion: Int = companion.Version

  def getInputFilter: InputFilter = this

  def getIndexer: DataIndexer[K, List[TextRange], FileContent] = this

  def getKeyDescriptor: KeyDescriptor[K] = companion.KeyDescriptor
  def getValueExternalizer: DataExternalizer[List[TextRange]] = new ListExternalizer(TextRangeExternalizer)

  def acceptInput(file: VirtualFile): Boolean =
    file.getFileType == HoconFileType

  def map(inputData: FileContent): JMap[K, List[TextRange]] = inputData.getPsiFile match {
    case hf: HoconPsiFile =>
      val result = new java.util.HashMap[K, List[TextRange]]
      hf.depthFirst.collectOnly[HKey].foreach { hkey =>
        indexKey(hkey).foreach { indexKey =>
          val prevRanges = result.getOrDefault(indexKey, Nil)
          result.put(indexKey, hkey.getTextRange :: prevRanges)
        }
      }
      result
    case _ =>
      Collections.emptyMap[K, List[TextRange]]
  }
}

object HoconPathIndex extends HKeyIndexCompanion[List[String]]("hoconPaths") {
  final val Version = 0

  object KeyDescriptor extends ListExternalizer[String](EnumeratorStringDescriptor.INSTANCE)
    with KeyDescriptor[List[String]] {

    def getHashCode(value: List[String]): Int = value.hashCode
    def isEqual(val1: List[String], val2: List[String]): Boolean = val1 == val2
  }
}

class HoconPathIndex extends HKeyIndex[List[String]](HoconPathIndex) {
  def indexKey(hkey: HKey): Option[List[String]] =
    hkey.fullValidContainingPath.map {
      case (_, path) => path.map(_.stringValue)
    }
}

object HoconKeyIndex extends HKeyIndexCompanion[String]("hoconKeys") {
  final val Version = 0
  final def KeyDescriptor: KeyDescriptor[String] = EnumeratorStringDescriptor.INSTANCE
}

class HoconKeyIndex extends HKeyIndex[String](HoconKeyIndex) {
  def indexKey(hkey: HKey): Option[String] = Some(hkey.stringValue)
}

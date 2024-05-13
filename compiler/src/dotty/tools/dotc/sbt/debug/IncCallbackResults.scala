package dotty.tools.dotc.sbt.debug

import scala.collection.immutable.Queue

import xsbti.api.ClassLike
import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.FileExtension
import dotty.tools.dotc.sbt.DefaultShowAPI

import java.nio.file.Path

object IncCallbackResults:
  trait IncResultEntry

  case class StartSource(sourceFile: SourceFile | Null) extends IncResultEntry
  case class Api(sourceFile: SourceFile | Null, classApi: ClassLike | Null) extends IncResultEntry:
    override def toString: String = s"Api($sourceFile, ${if classApi == null then "<null>" else DefaultShowAPI(classApi.nn)})"
  case class MainClass(sourceFile: SourceFile | Null, className: String | Null) extends IncResultEntry
  case class GeneratedNonLocalClass(source: SourceFile | Null, classFile: Path | Null, binaryClassName: String | Null, srcClassName: String | Null) extends IncResultEntry
  case object NoResult extends IncResultEntry
end IncCallbackResults

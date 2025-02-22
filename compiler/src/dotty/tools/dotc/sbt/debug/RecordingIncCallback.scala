package dotty.tools.dotc.sbt.debug

import dotty.tools.dotc.sbt.interfaces
import IncCallbackResults.*
import xsbti.api.ClassLike

import dotty.tools.dotc.util.SourceFile
import scala.collection.immutable.Queue
import java.nio.file.Path
import dotty.tools.dotc.sbt.interfaces.IncrementalCallback

class RecordingIncCallback extends interfaces.IncrementalCallback:
  var results: Map[StartSource, Queue[IncResultEntry]] = Map.empty.withDefaultValue(Queue.empty)
  var source: StartSource | Null = null

  private def record(op: IncResultEntry): Unit =
    if source == null then
      throw IllegalStateException("No source file set")

    val s = source.nn
    results = results + (s -> (results(s) :+ op))

  override def startSource(sourceFile: SourceFile | Null): Unit =
    source = StartSource(sourceFile)
    record(source.nn)

  override def api(sourceFile: SourceFile | Null, classApi: ClassLike | Null): Unit =
    record(Api(sourceFile, classApi))
    
  override def mainClass(sourceFile: SourceFile | Null, className: String | Null): Unit =
    record(MainClass(sourceFile, className))

  override def generatedNonLocalClass(source: SourceFile | Null, classFile: Path | Null, binaryClassName: String | Null, srcClassName: String | Null): Unit =
    record(GeneratedNonLocalClass(source, classFile, binaryClassName, srcClassName))

  def propagate(cb: IncrementalCallback | Null): Unit =
    if cb != null then
      for (source, resultEntries) <- results do
        for entry <- resultEntries do
          entry match
            case StartSource(sourceFile) => cb.startSource(sourceFile)
            case Api(sourceFile, classApi) => cb.api(sourceFile, classApi)
            case MainClass(sourceFile, className) => cb.mainClass(sourceFile, className)
            case GeneratedNonLocalClass(source, classFile, binaryClassName, srcClassName) => cb.generatedNonLocalClass(source, classFile, binaryClassName, srcClassName)
            case NoResult =>
  end propagate
end RecordingIncCallback
/*
Adapted from:
https://github.com/scalacenter/tasty-query/pull/371
https://github.com/scalacenter/scala-debug-adapter/blob/main/modules/decoder/src/main/scala/ch/epfl/scala/debugadapter/internal/stacktrace/CustomClasspath.scala
 */

package dotty.tools.dotc
package sbt

import tastyquery.Classpaths as tqcp
import tastyquery.Classpaths.{Classpath as TQClasspath, ClasspathEntry as TQClasspathEntry,
  PackageData as TQPackageData, ClassData as TQClassData}
import tastyquery.Contexts as tqctxs
import tastyquery.Contexts.{Context as TQContext, ctx as tqctx}

import dotty.tools.io.ClassPath
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.ScalacCommand
import dotty.tools.io.AbstractFile
import dotty.tools.dotc.classpath.PackageEntry
import dotty.tools.dotc.Compiler
import dotty.tools.dotc.classpath.AggregateClassPath
import dotty.tools.io.FileZipArchive

object LazyTastyQueryClasspath:
  class DotcEntry(debugName: String, cp: ClassPath) extends TQClasspathEntry:
    override def toString(): String = debugName

    def replaceSuffix(cls: AbstractFile, oldSuffix: String, newSuffix: String): Option[AbstractFile] =
      val dir = cls match
        case cls: FileZipArchive#Entry => cls.parent
        case cls: AbstractFile => cls.container
      val name = cls.name.stripSuffix(oldSuffix) + newSuffix
      Option(dir.lookupName(name, directory = false))

    lazy val _packages: List[DotcPackageData] =
      def loadClasses(pkgName: String): List[DotcClassData] =
        cp.classes(pkgName).toList.map(cls =>
          val binary = cls.binary
          val suffix = binary.flatMap(_.name.split('.').lastOption)
          val (classRaw, tastyRaw) = suffix match
            case Some("class") =>
              (binary, binary.flatMap(replaceSuffix(_, ".class", ".tasty")))
            case Some("tasty") =>
              (binary.flatMap(replaceSuffix(_, ".tasty", ".class")), binary)
            case _ =>
              (None, None)
          
          val clsFullName = if pkgName.isEmpty() then cls.name else s"${pkgName}.${cls.name}"
          DotcClassData(s"$debugName:$clsFullName", cls.name, classRaw, tastyRaw)
        )

      def loadPackage(name: String): DotcPackageData =
        DotcPackageData(s"$debugName:$name", name, () => loadClasses(name))

      def loadSubPackages(name: String): List[DotcPackageData] =
        cp.packages(name).toList.flatMap(pkg => loadPackage(pkg.name) :: loadSubPackages(pkg.name))
      
      val emptyPkgName = ""
      loadPackage(emptyPkgName) :: loadSubPackages(emptyPkgName)

    override def listAllPackages(): List[DotcPackageData] = _packages
  end DotcEntry

  class DotcPackageData(val debugName: String, override val dotSeparatedName: String, fetchClasses: () => List[DotcClassData]) extends TQPackageData:
    override def toString(): String = debugName

    private lazy val _classes: List[DotcClassData] =
      fetchClasses()

    private lazy val _byName: Map[String, DotcClassData] = _classes.map(cls => cls.binaryName -> cls).toMap

    override def listAllClassDatas(): List[DotcClassData] = _classes

    override def getClassDataByBinaryName(binaryName: String): Option[DotcClassData] = _byName.get(binaryName)
  end DotcPackageData


  def patchBytes(bytes: Array[Byte]): IArray[Byte] =
    bytes(4) = (28 | 0x80).toByte // major version
    bytes(5) = (4 | 0x80).toByte // minor version
    bytes(6) = (0 | 0x80).toByte // experimental
    IArray.unsafeFromArray(bytes)

  class DotcClassData(val debugName: String, override val binaryName: String, cls: Option[AbstractFile], tsty: Option[AbstractFile]) extends TQClassData:
    override def toString(): String = debugName

    private lazy val _tastyFileBytesPatched: IArray[Byte] = patchBytes(tsty.get.toByteArray)

    override def readClassFileBytes(): IArray[Byte] =
      IArray.unsafeFromArray(cls.get.toByteArray)

    override def hasClassFile: Boolean = cls.exists(_.exists)

    override def readTastyFileBytes(): IArray[Byte] =
      _tastyFileBytesPatched

    override def hasTastyFile: Boolean = tsty.exists(_.exists)
  end DotcClassData
  
  class InMemoryEntry(debugName: String, packages: List[InMemoryPackageData]) extends TQClasspathEntry:
    override def toString(): String = debugName

    override def listAllPackages(): List[InMemoryPackageData] = packages
  end InMemoryEntry
  
  class InMemoryPackageData(val debugName: String, override val dotSeparatedName: String, fetchClasses: () => List[InMemoryTasty]) extends TQPackageData:
    override def toString(): String = debugName

    private lazy val _classes: List[InMemoryTasty] = fetchClasses()

    private lazy val _byName: Map[String, InMemoryTasty] = _classes.map(cls => cls.binaryName -> cls).toMap

    override def listAllClassDatas(): List[InMemoryTasty] = _classes

    override def getClassDataByBinaryName(binaryName: String): Option[InMemoryTasty] = _byName.get(binaryName)
  end InMemoryPackageData

  class InMemoryTasty(val debugName: String, override val binaryName: String, val tsty: () => Array[Byte]) extends TQClassData:
    override def toString(): String = debugName

    lazy val _tastyFileBytesPatched: IArray[Byte] = patchBytes(tsty().clone())
    
    override def readClassFileBytes(): IArray[Byte] = IArray.empty

    override def hasClassFile: Boolean = false

    override def hasTastyFile: Boolean = true

    override def readTastyFileBytes(): IArray[Byte] = _tastyFileBytesPatched
  end InMemoryTasty


  def makeClasspath(using Context): TQClasspath =
    def flattenClasspath(cp: ClassPath): List[ClassPath] = cp match
      case ag: AggregateClassPath => ag.aggregates.flatMap(flattenClasspath).toList
      case _ => List(cp)
    val cp = ctx.base.platform.classPath
    flattenClasspath(ctx.base.platform.classPath).map(cp => DotcEntry(cp.asClassPathString, cp))
  
end LazyTastyQueryClasspath

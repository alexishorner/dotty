package tastyquery

import Contexts.*
import Symbols.*
import Types.*
import Names.*
import Modifiers.*

import Bridge.*

import scala.annotation.tailrec

object CommonNames:
  val main: TermName = termName("main")
  val pkg: TermName = termName("package")
end CommonNames

object Extensions: 
  extension (name: Name)(using Context)
    def toTermName: TermName = name match
      case name: TypeName => name.toTermName
      case name: TermName => name
    end toTermName

    def toTypeName: TypeName = name match
      case name: TypeName => name
      case name: TermName => name.toTypeName
    end toTypeName

    // cannot use function name `stripModuleClassSuffix`, because it is already used in
    // a `NameOps` extension
    def stripModuleClassSuffix2: Name = name match
      case ObjectClassTypeName(underlying) => underlying
      case _ => name
    end stripModuleClassSuffix2

    def mangledString: String = name match
      case name: TermName => name.mangledString
      case name: TypeName => name.mangledString
    end mangledString

    // def isPackageObjectClassName: Boolean = name match
    //   case ObjectClassTypeName(objName) => objName.toTermName.isPackageObjectName
    //   case _                            => false
    // end isPackageObjectClassName

    // /** is this the name of an object enclosing package-level definitions? */
    // def isPackageObjectName: Boolean = name match
    //   case name: TermName => name == CommonNames.pkg/* || name.endsWith("$package")*/
    //   case name: TypeName => name.isPackageObjectClassName
    // end isPackageObjectName
  end extension

  extension (name: TypeName)(using Context)
    def mangledString: String = name.toTermName.mangledString
  end extension

  extension (name: TermName)(using Context)
    def mangledString: String = name.toString // TODO implement
  end extension

  // extension (name: SimpleName)(using Context)
  //   def isPackageObjectName: Boolean =
  //     name.name == "package" || name.name.endsWith("$package")
  //   end isPackageObjectName
  // end extension
  
  extension (sym: Symbol)(using Context)
    // TODO add separator
    def fullName: String =
      val b = new StringBuilder
      def build(sym: Symbol): Unit = {
        if (sym.owner != null)
          build(sym.owner)
          b append "."
        b append sym.name 
      }
      build(sym)
      b.toString
    end fullName

    def fullNameNoModuleClassSuffix: String =
      @tailrec
      def names(sym: Symbol, acc: List[Name]): List[Name] =
        val newAcc = sym.name.stripModuleClassSuffix2 :: acc
        if sym.owner == null then
          newAcc
        else
          names(sym.owner, newAcc)
      names(sym, Nil).filter(_ != nme.RootName).mkString(".")
    end fullNameNoModuleClassSuffix

    def zincMangledName: String =
      // if sym.isConstructor  then
      //   // TODO: ideally we should avoid unnecessarily caching these Zinc specific
      //   // names in the global chars array. But we would need to restructure
      //   // ExtractDependencies caches to avoid expensive `toString` on
      //   // each member reference.
      //   termName(sym.owner.nn.fullName.mangledString.replace(".", ";").nn ++ ";init;")
      // else
      //   sym.name.stripModuleClassSuffix
      sym.name.stripModuleClassSuffix2.toString // TODO implement

    def thisType: Prefix = sym match
      case sym: ClassSymbol => sym.thisType
      case _ => NoPrefix
    end thisType

    /** The chain of owners of this denotation, starting with the denoting symbol itself */
    def ownersIterator: Iterator[Symbol] = new Iterator[Symbol] {
      private var current: Symbol | Null = sym
      def hasNext = current != null
      def next: Symbol = {
        val result = current.nn
        current = result.owner
        result
      }
    }

    /** Is this symbol the root class or its companion object? */
    def isRoot: Boolean =
      (sym.owner == null) && sym.name.toTermName == Names.nme.RootName || sym.name == Names.nme.UserLandRootPackageName

    /** Is this symbol the empty package class or its companion object? */
    def isEmptyPackage: Boolean =
      val owner = sym.owner
      sym.name.toTermName == Names.nme.EmptyPackageName && owner != null && owner.isRoot
    end isEmptyPackage

    // /** Is this symbol a package object or its module class? */
    // def isPackageObject: Boolean =
    //   sym.name.isPackageObjectName && owner.is(Package) && this.is(Module)
    // end isPackageObject

    /** Is this symbol the empty package class or its companion object? */
    def isEffectiveRoot: Boolean = sym.isRoot || sym.isEmptyPackage

    def isConstructor: Boolean =
      sym.name == nme.Constructor // FIXME why do we need `Names` prefix ?
    end isConstructor
    
    def isTopLevelClass: Boolean =
      true // TODO implement (owner is package)
    end isTopLevelClass

    def hack_isOverride: Boolean =
      sym.is(Override)
    end hack_isOverride

    /* Public equivalent of `Symbol.isStatic`. We need another name, because the extension cannot have the same name as a method. */
    def hack_isStatic: Boolean =
      sym.isStatic
    end hack_isStatic

    private inline def predicateAs[T <: Symbol](inline p: T => Boolean): Boolean = (sym: @unchecked) match
        case sym: T => p(sym)
        case _ => false
    end predicateAs

    // `ClassSymbol` predicates
    def isTrait: Boolean =
      predicateAs[ClassSymbol](_.isTrait)
    end isTrait

    def isAbstractClass: Boolean =
      predicateAs[ClassSymbol](_.isAbstractClass)
    end isAbstractClass
    
    private inline def hasOpenLevel(inline level: OpenLevel): Boolean =
      predicateAs[ClassSymbol](_.openLevel == level)
    end hasOpenLevel

    def isFinal: Boolean =
      hasOpenLevel(OpenLevel.Final)
    end isFinal
    
    def isSealed: Boolean =
      hasOpenLevel(OpenLevel.Sealed)
    end isSealed

    // `TermSymbol` predicates

    def isAbstractOverride: Boolean =
      predicateAs[TermSymbol](_.isAbstractOverride)
    end isAbstractOverride

    def isAbstractMember: Boolean =
      predicateAs[TermSymbol](_.isAbstractMember)
    end isAbstractMember

    def isGivenOrUsing: Boolean =
      predicateAs[TermSymbol](_.isGivenOrUsing)
    end isGivenOrUsing

    def isImplicit: Boolean =
      predicateAs[TermSymbol](_.isImplicit)
    end isImplicit

    private inline def hasKind(kind: TermSymbolKind): Boolean =
      predicateAs[TermSymbol](_.kind == kind)
    end hasKind

    def isLazyVal: Boolean =
      hasKind(TermSymbolKind.LazyVal)
    end isLazyVal

    def isMacro: Boolean =
      predicateAs[TermSymbol](_.isMacro)
    end isMacro

    def isInline: Boolean =
      predicateAs[TermSymbol](_.isInline)
    end isInline

    def isParamWithDefault: Boolean =
      predicateAs[TermSymbol](_.isParamWithDefault)
    end isParamWithDefault

  end extension

  extension(sym: ClassSymbol)(using Context)
    def typeRef: TypeRef =
      TypeRef(sym.owner.thisType, sym)
    end typeRef

    /** Recursively assemble all children of this symbol, Preserves order of insertion.
     */
    def sealedStrictDescendants: List[ClassSymbol | TermSymbol] =
      import scala.collection.mutable
      import dotty.tools.dotc.util
      import scala.annotation.tailrec

      @tailrec
      def findLvlN(
        explore: mutable.ArrayDeque[ClassSymbol | TermSymbol],
        seen: util.HashSet[ClassSymbol | TermSymbol],
        acc: mutable.ListBuffer[ClassSymbol | TermSymbol]
      ): List[ClassSymbol | TermSymbol] =
        if explore.isEmpty then
          acc.toList
        else
          val sym      = explore.head
          val explore1 = explore.dropInPlace(1)
          val lvlN     = sym match
            case sym: ClassSymbol => sym.sealedChildren
            case _                => Nil
          // val notSeen  = lvlN.filterConserve(!seen.contains(_))
          val notSeen  = lvlN.filter(!seen.contains(_))
          if notSeen.isEmpty then
            findLvlN(explore1, seen, acc)
          else
            findLvlN(explore1 ++= notSeen, {seen ++= notSeen; seen}, acc ++= notSeen)
      end findLvlN

      /** Scans through `explore` to see if there are recursive children.
       *  If a symbol in `explore` has children that are not contained in
       *  `lvl1`, fallback to `findLvlN`, or else return `lvl1`.
       */
      @tailrec
      def findLvl2(
        lvl1: List[ClassSymbol | TermSymbol], explore: List[ClassSymbol | TermSymbol], seenOrNull: util.HashSet[ClassSymbol | TermSymbol] | Null
      ): List[ClassSymbol | TermSymbol] = explore match
        case sym :: explore1 =>
          val lvl2 = sym match
            case sym: ClassSymbol => sym.sealedChildren
            case _                => Nil
          if lvl2.isEmpty then // no children, scan rest of explore1
            findLvl2(lvl1, explore1, seenOrNull)
          else // check if we have seen the children before
            val seen = // initialise the seen set if not already
              if seenOrNull != null then seenOrNull
              else util.HashSet.from(lvl1)
            // val notSeen = lvl2.filterConserve(!seen.contains(_))
            val notSeen = lvl2.filter(!seen.contains(_))
            if notSeen.isEmpty then // we found children, but we had already seen them, scan the rest of explore1
              findLvl2(lvl1, explore1, seen)
            else // found unseen recursive children, we should fallback to the loop
              findLvlN(
                explore = mutable.ArrayDeque.from(explore1).appendAll(notSeen),
                seen = {seen ++= notSeen; seen},
                acc = mutable.ListBuffer.from(lvl1).appendAll(notSeen)
              )
        case nil =>
          lvl1
      end findLvl2

      val lvl1 = sym.sealedChildren
      findLvl2(lvl1, lvl1, seenOrNull = null)

    def sealedDescendants: List[ClassSymbol | TermSymbol] =
      // sym :: sym.sealedChildren // TODO reimplement like in dotty
      sym :: sym.sealedStrictDescendants
    end sealedDescendants

    def hasMainMethod: Boolean =
      sym.getMember(CommonNames.main) match
        case Some(main) =>
          main.isMainMethod && (sym.isModuleClass || sym.isStatic)
        case None => false
      end match
    end hasMainMethod

    def hack_isDerivedValueClass: Boolean =
      sym.isDerivedValueClass
    end hack_isDerivedValueClass
  end extension

  extension(sym: TermSymbol)(using Context)
    def termRef: TermRef =
      TermRef(sym.owner.thisType, sym)
    end termRef

    def isMainMethod: Boolean =
      (sym.name == CommonNames.main) && (sym.declaredType match {
        case MethodTypeExtractor(_, List(el), restpe) =>
          (el isSameType defn.StringType) && (restpe isTypeRefOf defn.UnitClass)
        case _ => false
      })
    end isMainMethod
  end extension

  extension (tpe: TypeOrMethodic)(using Context)
    /** Is this type a (neither aliased nor applied nor annotated) reference to class `sym`? */
    def isDirectRef(sym: Symbol): Boolean = tpe match // TODO check stripTypeVar
      case this1: TypeRef =>
        this1.name == sym.name
        // this1.name == sym.name && // avoid forcing resolve if names differ
        // this1.optSymbol.exists(_ eq sym)
      case _ =>
        false
    end isDirectRef

    def isTypeRefOf(cls: ClassSymbol): Boolean = tpe match
      case TypeRef.OfClass(tpeCls) => tpeCls == cls
      case _                       => false
    end isTypeRefOf

    /** Strip PolyType prefixes */
    def stripPoly: TypeOrMethodic = tpe match
      case tp: PolyType => tp.resultType.stripPoly
      case _ => tpe
    end stripPoly

    /** The final result type of a PolyType, MethodType, or ExprType, after skipping
     *  all parameter sections, the type itself for all others.
     */
    def finalResultType: TypeOrMethodic =
      val resultType = tpe match
        case lt: LambdaType => lt.resultType
        case _ => tpe
      
      resultType.stripPoly match
        case mt: MethodType => mt.resultType.finalResultType
        case _ => resultType
    end finalResultType

    def widenExpr: TypeOrMethodic = tpe match
      case tp: ByNameType => tp.resultType
      case _ => tpe
    end widenExpr
  end extension

  extension (tpe: Type)(using Context)
    def isRef(sym: TypeSymbol): Boolean = tpe match
      case tpe: TypeRef => tpe.optSymbol.contains(sym)
      case _            => false
    end isRef

    def isRef(sym: TermSymbol): Boolean = tpe match
      case tpe: TermRef => tpe.symbol == sym
      case _            => false
    end isRef

    def isAny: Boolean = tpe.isRef(defn.AnyClass)

    def isNothing: Boolean = tpe.dealias.isInstanceOf[NothingType]
  end extension

  extension (bounds: TypeBounds)(using Context)
    def isNothingAnyBounds: Boolean =
      bounds.low.isNothing && bounds.high.isAny
    end isNothingAnyBounds
  end extension

  private object MethodTypeExtractor:
    def unapply(mt: MethodType): Some[(List[TermName], List[Type], TypeOrMethodic)] =
      Some((mt.paramNames, mt.paramInfos, mt.resultType))
    end unapply
  end MethodTypeExtractor
end Extensions

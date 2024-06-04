package tastyquery

import Contexts.*
import Symbols.*
import Types.*
import Names.*
import Modifiers.*
import Trees.*

import Bridge.*

import scala.annotation.tailrec

object CommonNames:
  val main: SimpleName = termName("main")
  val pkg: TermName = termName("package")
end CommonNames

object MyDefinitions:
  def internalBodyAnnotClass(using Context) =
    defn.scalaAnnotationInternalPackage.getDecl(typeName("Body")).get.asClass
  
  def internalChildAnnotClass(using Context) =
    defn.internalChildAnnotClass.get
end MyDefinitions

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
    @tailrec
    private def fullNameList(currentSym: Symbol = sym, acc: List[Name] = Nil): List[Name] =
      val name = currentSym.name
      val newAcc =
        if name == nme.RootName || name == nme.EmptyPackageName then
          acc
        else currentSym.name :: acc
      if currentSym.owner == null then
        newAcc
      else
        fullNameList(currentSym.owner, newAcc)
    end fullNameList

    // TODO add separator
    def fullName: String =
      // val b = new StringBuilder
      // def build(sym: Symbol): Unit = {
      //   if (sym.owner != null)
      //     build(sym.owner)
      //     b append "."
      //   b append sym.name 
      // }
      // build(sym)
      // b.toString
      fullNameList().mkString(".")
    end fullName

    def fullNameNoModuleClassSuffix: String =
      // sym.fullNameList().map(_.stripModuleClassSuffix2).filter(_ != nme.RootName).mkString(".")
      sym.fullNameList().map(_.stripModuleClassSuffix2).mkString(".")
    end fullNameNoModuleClassSuffix

    def zincMangledName: Name =
      if sym.isConstructor  then
        // TODO: ideally we should avoid unnecessarily caching these Zinc specific
        // names in the global chars array. But we would need to restructure
        // ExtractDependencies caches to avoid expensive `toString` on
        // each member reference.
        termName(sym.owner.nn.fullNameList().map(_.mangledString).mkString(";") ++ ";init;")
      else
        sym.name.stripModuleClassSuffix2
      // sym.name.stripModuleClassSuffix2.toString // TODO implement

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

    /** `sym` is an inline method with a known body to inline.
     */
    def hack_hasBodyToInline: Boolean =
      sym.isInline && sym.isMethod/* && sym.hasAnnotation(MyDefinitions.internalBodyAnnotClass)*/

    /** The body to inline for method `sym`, or `EmptyTree` if none exists.
     *  @pre  hasBodyToInline(sym)
     */
    def hack_bodyToInline: Option[Tree] =
      if sym.hack_hasBodyToInline then
        // Some(sym.getAnnotation(MyDefinitions.internalBodyAnnotClass).get.tree)
        Some(sym.tree.get.asInstanceOf[DefDef].rhs.get)
      else
        None

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
    
    def isTopLevel: Boolean =
      val owner = sym.owner
      owner != null && owner.isPackage
    end isTopLevel

    def hack_isOverride: Boolean =
      sym.is(Override)
    end hack_isOverride

    /* Public equivalent of `Symbol.isStatic`. We need another name, because the extension cannot have the same name as a method. */
    def hack_isStatic: Boolean =
      sym.isStatic
    end hack_isStatic

    /** Is this symbol directly owner by a term symbol, i.e., is it local to a block? */
    def isLocalToBlock: Boolean =
      val owner = sym.owner
      owner != null && owner.isTerm

    /** Is symbol directly or indirectly owned by a term symbol? */
    @tailrec final def isLocal: Boolean = {
      val owner = sym.owner
      if (owner == null) false
      else if (isLocalToBlock) true
      else if (owner.isPackage) false
      else owner.isLocal
    }

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

    def isFinal: Boolean = sym match
      case sym: ClassSymbol => sym.openLevel == OpenLevel.Final
      case sym: TermOrTypeSymbol => sym.isFinalMember
      case _ => false
    end isFinal
    
    def isSealed: Boolean =
      hasOpenLevel(OpenLevel.Sealed)
    end isSealed

    def hack_isPackageClass: Boolean =
      sym.isPackage // FIXME difference between Package and PackageClass
      // predicateAs[ClassSymbol](sym => 
      //   sym.isTopLevel && sym.isModuleClass && sym.name.isPackageObjectClassName
      // )
    end hack_isPackageClass

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

    def isMethod: Boolean =
      predicateAs[TermSymbol](_.isMethod)
    end isMethod

    def isParamWithDefault: Boolean =
      predicateAs[TermSymbol](_.isParamWithDefault)
    end isParamWithDefault

    def isParamAccessor: Boolean =
      predicateAs[TermSymbol](_.isParamAccessor)
    end isParamAccessor
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

    def hack_findMember(pred: TermOrTypeSymbol => Boolean): Option[TermOrTypeSymbol] =
      @tailrec
      def lookup(lin: List[ClassSymbol]): Option[TermOrTypeSymbol] = lin match
        case parentCls :: linRest =>
          val res = parentCls.declarations.find(sym => !sym.isPrivate && pred(sym))
          if res.isDefined then res
          else lookup(linRest)
        case Nil =>
          None
      end lookup

      sym.declarations.find(pred).orElse(
        lookup(sym.linearization.tail)
      )
    end hack_findMember

    def hasMainMethod: Boolean =
      import Signatures.*
      // val arraySignatureName = defn.ArrayClass.signatureName
      // val main = SignedName(CommonNames.main, Signature(List(ParamSig.Term(arraySignatureName)), defn.UnitClass.signatureName))
      // sym.getMember(main) match
      //   case Some(main) =>
      //     main.isMainMethod && (sym.isModuleClass || sym.isStatic)
      //   case None => false
      // end match

      // TODO implement with getMember or getDecl
      (sym.isModuleClass || sym.isStatic) &&
        sym.hack_findMember {
          case decl: TermSymbol => decl.isMainMethod
          case _ => false
        }.isDefined
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
          (el isSameType defn.ArrayTypeOf(defn.StringType)) && (restpe isTypeRefOf defn.UnitClass)
        case _ => false
      })
    end isMainMethod
  end extension

  extension (tp: TypeMappable)(using Context)
    def hack_isLambdaSub: Boolean = tp match
      case tp: Type => tp.isLambdaSub
      case _ => false
    end hack_isLambdaSub

    def hack_dealiasKeepAnnots: TypeMappable = tp match
      case tp: Type =>
        tp.hack_dealiasKeepAnnots
      case _ => tp
    end hack_dealiasKeepAnnots
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
    def hack_dealiasKeepAnnots: Type = tpe match
      case tp: TypeRef =>
        tp.optSymbol match
          case Some(tpSym: TypeMemberSymbol) =>
            tpSym.typeDef match
              case TypeMemberDefinition.TypeAlias(alias)          => alias.hack_dealiasKeepAnnots
              case TypeMemberDefinition.OpaqueTypeAlias(_, alias) => alias.hack_dealiasKeepAnnots
              case _                                              => tp
          case _ =>
            tp.optAliasedType match
              case Some(alias) => alias.hack_dealiasKeepAnnots
              case None        => tp
      case tp: AppliedType =>
        val tycon1 = tp.tycon.hack_dealiasKeepAnnots
        if (tycon1 ne tp.tycon) || tycon1.isInstanceOf[TypeLambda] then tp.superType.hack_dealiasKeepAnnots
        else tpe
      case tp: AnnotatedType =>
        val typ = tp.typ.hack_dealiasKeepAnnots
        tp.derivedAnnotatedType(typ, tp.annotation)
      case _ =>
        tpe
    end hack_dealiasKeepAnnots

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

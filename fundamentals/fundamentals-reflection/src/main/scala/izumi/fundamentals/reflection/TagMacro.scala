package izumi.fundamentals.reflection

import izumi.fundamentals.platform.console.TrivialLogger
import izumi.fundamentals.reflection.ReflectionUtil.{Kind, kindOf}
import izumi.fundamentals.reflection.Tags.{defaultTagImplicitError, hktagFormat, hktagFormatMap}
import izumi.fundamentals.reflection.macrortti.LightTypeTagMacro0

import scala.annotation.{implicitNotFound, tailrec}
import scala.collection.immutable.ListMap
import scala.reflect.macros.{TypecheckException, blackbox, whitebox}

// TODO: benchmark difference between running implicit search inside macro vs. return tree with recursive implicit macro expansion
// TODO: benchmark difference between searching all arguments vs. merge strategy
// TODO: benchmark ProviderMagnet vs. identity macro vs. normal function
class TagMacro(val c: blackbox.Context) {

  import c.universe._

  protected[this] val logger: TrivialLogger = TrivialMacroLogger.make[this.type](c, DebugProperties.`izumi.debug.macro.rtti`)

  /**
    * Workaround for a scalac bug whereby it loses the correct type of HKTag argument
    * Here, if implicit resolution fails because scalac thinks that `ArgStruct` is a WeakType,
    * we just inspect it and recreate HKTag Arg again.
    *
    * See: TagTest, "scalac bug: can't find HKTag when obscured by type lambda"
    *
    * TODO: report scalac bug
    */
  def fixupHKTagArgStruct[DIU <: Tags with Singleton : WeakTypeTag, T: WeakTypeTag]: c.Expr[DIU#HKTag[T]] = {
    val argStruct = weakTypeOf[T]
    val typeConstructor = argStruct.decls.head.info.typeConstructor

    logger.log(s"HKTag fixup: got arg struct $argStruct, type constructor $typeConstructor")

    val newHkTag = mkHKTagArg(typeConstructor, kindOf(typeConstructor))

    val res = summonTypeTag[DIU, T](newHkTag)

    logger.log(s"resulting ArgStruct TypeTag $res")

    val ltagMacro = new LightTypeTagMacro0[c.type](c)
    val lhktag = ltagMacro.makeHKTagRaw[T](newHkTag)

    logger.log(s"resulting LHKTag $lhktag")

    reify {
      c.prefix.asInstanceOf[c.Expr[DIU#HKTagObject]].splice.apply[T](res.splice, lhktag.splice)
    }
  }

  // workaround for a scalac bug - `Nothing` type is lost when two implicits for it are summoned from one implicit as in:
  //  implicit final def tagFromTypeTag[T](implicit t: TypeTag[T], l: LTag[T]): Tag[T] = Tag(t, l.fullLightTypeTag)
  // https://github.com/scala/bug/issues/11715
  def FIXMEgetLTagAlso[DIU <: Tags with Singleton, T](t: c.Expr[DIU#ScalaReflectTypeTag[T]])(implicit w: c.WeakTypeTag[T]): c.Expr[DIU#Tag[T]] = {
    val ltagMacro = new LightTypeTagMacro0[c.type](c)
    val ltag = ltagMacro.makeParsedLightTypeTag[T](w)
    c.Expr[DIU#Tag[T]] {
      q"${c.prefix.asInstanceOf[Expr[DIU#TagObject]]}.apply[$w]($t, $ltag)"
    }
  }

  def impl[DIU <: Tags with Singleton : c.WeakTypeTag, T: c.WeakTypeTag]: c.Expr[TagMaterializer[DIU, T]] = {

    logger.log(s"GOT UNIVERSE: ${c.weakTypeOf[DIU]}")
    logger.log(s"Got compile tag: ${weakTypeOf[T]}")

    if (getImplicitError[DIU]().endsWith(":")) { // yep
      logger.log(s"Got continuation implicit error: ${getImplicitError[DIU]()}")
    } else {
      resetImplicitError[DIU]()
      addImplicitError("\n\n<trace>: ")
    }

    val tgt = norm(weakTypeOf[T].dealias)

    val universe = universeSingleton[DIU]
    logger.log(s"Universe object: ${showCode(universe.tree)}")

    addImplicitError(s"  deriving Tag for $tgt:")

    val tag = tgt match {
      case RefinedType(intersection, _) =>
        mkRefined[DIU, T](universe, intersection, tgt)
      case _ =>
        mkTag[DIU, T](universe, tgt)
    }

    val res = reify {
      {new TagMaterializer[DIU, T](tag.splice)}
    }

    addImplicitError(s"  done: $tgt")

    logger.log(s"Final code of TagMaterializer[${weakTypeOf[T]}]:\n ${showCode(res.tree)}")

    res
  }

  def getImplicitError[DIU <: Tags with Singleton : c.WeakTypeTag](): String = {
    symbolOf[DIU#Tag[Any]].annotations.headOption.flatMap(
      AnnotationTools.findArgument(_) {
        case Literal(Constant(s: String)) => s
      }
    ).getOrElse(defaultTagImplicitError)
  }

  @inline
  protected[this] def mkRefined[DIU <: Tags with Singleton : c.WeakTypeTag, T: c.WeakTypeTag](universe: c.Expr[DIU], intersection: List[Type], struct: Type): c.Expr[DIU#Tag[T]] = {
    val intersectionsTags = c.Expr[List[DIU#ScalaReflectTypeTag[_]]](q"${
      intersection.map {
        t0 =>
          val t = norm(t0.dealias)
          summonTypeTagFromTag[DIU](t)
      }
    }")
    val structTag = mkStruct[DIU](struct)

    reify {
      {universe.splice.Tag.refinedTag[T](intersectionsTags.splice, structTag.splice)}
    }
  }

  @inline
  // have to tag along the original intersection, because scalac dies on trying to summon typetag for a custom made refinedType from `internal.refinedType` ...
  protected[this] def mkStruct[DIU <: Tags with Singleton : c.WeakTypeTag](struct: Type): c.Expr[DIU#ScalaReflectWeakTypeTag[_]] = {

    struct.decls.find(_.info.typeSymbol.isParameter).foreach {
      s =>
        val msg = s"  Encountered a type parameter ${s.info} as a part of structural refinement of $struct: It's not yet supported to summon a Tag for ${s.info} in that position!"

        addImplicitError(msg)
        c.abort(s.pos, msg)
    }

    // TODO: replace types of members with Nothing, in runtime replace types to types from tags searching by TermName("")
    summonWeakTypeTag[DIU](struct)
  }

  // we need to handle four cases – type args, refined types, type bounds and bounded wildcards(? check existence)
  @inline
  protected[this] def mkTag[DIU <: Tags with Singleton : c.WeakTypeTag, T: c.WeakTypeTag](universe: c.Expr[DIU], tpe: c.Type): c.Expr[DIU#Tag[T]] = {

    val ctor = tpe.typeConstructor
    val constructorTag: c.Expr[DIU#ScalaReflectTypeTag[_]] = typeParameterKind(ctor) match {
      case None => // concrete type
        val tpeToNothings = applyToNothings(tpe)
        logger.log(s"Type after replacing with Nothing $tpeToNothings, replaced args ${tpe.typeArgs}")
        summonTypeTag[DIU, Any](tpeToNothings)
      case Some(Kind(Nil)) => // error: lower-kinded type parameter in constructor position
        val msg = s"  could not find implicit value for ${hktagFormat(tpe)}: $tpe is a type parameter without an implicit Tag or TypeTag!"
        addImplicitError(msg)
        c.abort(c.enclosingPosition, msg)
      case Some(kind) =>
        summonTypeTagFromTagWithKind[DIU](ctor, kind)
    }

    val args = tpe.typeArgs.map {
      t => norm(t.dealias)
    }
    val argTags = c.Expr[List[DIU#ScalaReflectTypeTag[_]]](q"${args.map(summonTypeTagFromTag[DIU])}")

    reify {
      {universe.splice.Tag.appliedTag[T](constructorTag.splice, argTags.splice)}
    }
  }

  @inline
  protected[this] def typeParameterKind(tpe: c.Type): Option[Kind] = {
    // c.internal.isFreeType ?
    if (tpe.typeSymbol.isParameter)
      Some(kindOf(tpe))
    else
      None
  }

  @inline
  protected[this] def applyToNothings(tpe: c.Type): c.Type = {
    c.universe.appliedType(tpe, tpe.typeArgs.map(_ => definitions.NothingTpe))
  }

  protected[this] def mkTypeParameter(owner: Symbol, kind: Kind): Symbol = {
    import internal.reificationSupport._
    import internal.{polyType, typeBounds}

    val tpeSymbol = newNestedSymbol(owner, freshTypeName(""), NoPosition, Flag.PARAM | Flag.DEFERRED, isClass = false)

    val tpeTpe = if (kind.args.nonEmpty) {
      val params = kind.args.map(mkTypeParameter(tpeSymbol, _))

      polyType(params, typeBounds(definitions.NothingTpe, definitions.AnyTpe))
    } else {
      typeBounds(definitions.NothingTpe, definitions.AnyTpe)
    }

    setInfo(tpeSymbol, tpeTpe)

    tpeSymbol
  }

  @inline
  protected[this] def mkHKTagArg(tpe: c.Type, kind: Kind): Type = {

    import internal.reificationSupport._

    val staticOwner = c.prefix.tree.symbol.owner

    logger.log(s"staticOwner: $staticOwner")

    val parents = List(definitions.AnyRefTpe)
    val mutRefinementSymbol: Symbol = newNestedSymbol(staticOwner, TypeName("<refinement>"), NoPosition, FlagsRepr(0L), isClass = true)

    val mutArg: Symbol = newNestedSymbol(mutRefinementSymbol, TypeName("Arg"), NoPosition, FlagsRepr(0L), isClass = false)
    val params = kind.args.map(mkTypeParameter(mutArg, _))
    setInfo(mutArg, mkPolyType(tpe, params))

    val scope = newScopeWith(mutArg)

    setInfo[Symbol](mutRefinementSymbol, RefinedType(parents, scope, mutRefinementSymbol))

    RefinedType(parents, scope, mutRefinementSymbol)
  }

  @inline
  protected[this] def mkPolyType(tpe: c.Type, params: List[c.Symbol]): Type = {
    val rhsParams = params.map(internal.typeRef(NoPrefix, _, Nil))

    internal.polyType(params, appliedType(tpe, rhsParams))
  }

  @inline
  protected[this] def summonTypeTag[DIU <: Tags with Singleton : c.WeakTypeTag, T](tpe: c.Type): c.Expr[DIU#ScalaReflectTypeTag[T]] = {
    c.Expr[DIU#ScalaReflectTypeTag[T]](q"_root_.scala.Predef.implicitly[${appliedType(weakTypeOf[DIU#ScalaReflectTypeTag[Nothing]], tpe)}]")
  }

  @inline
  protected[this] def summonWeakTypeTag[DIU <: Tags with Singleton : c.WeakTypeTag](tpe: c.Type): c.Expr[DIU#ScalaReflectWeakTypeTag[_]] = {
    c.Expr[DIU#ScalaReflectWeakTypeTag[_]](q"_root_.scala.Predef.implicitly[${appliedType(weakTypeOf[DIU#ScalaReflectWeakTypeTag[Nothing]], tpe)}]")
  }

  @inline
  protected[this] def summonTypeTagFromTag[DIU <: Tags with Singleton : c.WeakTypeTag](tpe: c.Type): c.Expr[DIU#ScalaReflectTypeTag[_]] = {
    summonTypeTagFromTagWithKind[DIU](tpe, kindOf(tpe))
  }

  @inline
  protected[this] def summonTypeTagFromTagWithKind[DIU <: Tags with Singleton : c.WeakTypeTag](tpe: c.Type, kind: Kind): c.Expr[DIU#ScalaReflectTypeTag[_]] = {
    // dynamically typed
    val summoned = {
      try {
        if (kind == Kind(Nil)) {
          c.inferImplicitValue(appliedType(weakTypeOf[DIU#Tag[Nothing]].typeConstructor, tpe), silent = false)
        } else {
          val Arg = mkHKTagArg(tpe, kind)
          logger.log(s"Created impicit Arg: $Arg")
          c.inferImplicitValue(appliedType(weakTypeOf[DIU#HKTag[Nothing]].typeConstructor, Arg), silent = false)
        }
      } catch {
        case _: TypecheckException =>
          val msg =
            s"""  could not find implicit value for ${hktagFormat(tpe)}
               |${
              hktagFormatMap.get(kind) match {
                case Some(_) => ""
                case None =>
                  val (args, params) = kind.args.zipWithIndex.map {
                    case (k, i) =>
                      val name = s"T${i + 1}"
                      k.format(name) -> name
                  }.unzip
                  s"""\n$tpe is of a kind $kind, which doesn't have a tag name. Please create a tag synonym as follows:\n\n
                     |  type TagXXX[${kind.format("K")}] = HKTag[ { type Arg[${args.mkString(", ")}] = K[${params.mkString(", ")}] } ]\n\n
                     |And use it in your context bound, as in def x[$tpe: TagXXX] = ...
               """.stripMargin
              }
            }""".stripMargin
          addImplicitError(msg)
          c.abort(c.enclosingPosition, msg)
      }
    }

    c.Expr[DIU#ScalaReflectTypeTag[_]](q"{$summoned.tpe}")
  }

  /** Mini `normalize`. We don't wanna do scary things such as beta-reduce. And AFAIK the only case that can make us
    * confuse a type-parameter for a non-parameter is an empty refinement `T {}`. So we just strip it when we get it. */
  @tailrec
  protected[this] final def norm(x: Type): Type = {
    x match {
      case RefinedType(t :: Nil, m) if m.isEmpty =>
        logger.log(s"Stripped empty refinement of type $t. member scope $m")
        norm(t)
      case AnnotatedType(_, t) => norm(t)
      case _ => x
    }
  }

  @inline
  protected[this] def universeSingleton[DIU: c.WeakTypeTag]: c.Expr[DIU] = {
    val value = c.weakTypeOf[DIU] match {
      case u: SingleTypeApi =>
        u.sym.asTerm
      case u => c.abort(c.enclosingPosition,
        s"""Got a non-singleton universe type - $u. Please instantiate universe as
           | a val or an object and keep it somewhere in scope!!""".stripMargin)
    }
    c.Expr[DIU](q"$value")
  }

  @inline
  protected[this] def addImplicitError[DIU <: Tags with Singleton : c.WeakTypeTag](err: String): Unit = {
    setImplicitError(s"${getImplicitError()}\n$err")
  }

  @inline
  protected[this] def resetImplicitError[DIU <: Tags with Singleton : c.WeakTypeTag](): Unit = {
    setImplicitError[DIU](defaultTagImplicitError)
  }

  @inline
  protected[this] def setImplicitError[DIU <: Tags with Singleton : c.WeakTypeTag](err: String): Unit = {
    import internal.decorators._

    val _ = symbolOf[DIU#Tag[Any]].setAnnotations(Annotation(typeOf[implicitNotFound], List[Tree](Literal(Constant(err))), ListMap.empty))
  }

//  // TODO: in 2.13 we can use these little functions to enrich error messages further (possibly remove .setAnnotation hack completely) by attaching implicitNotFound to parameter
//  c.Expr[DIU#ScalaReflectTypeTag[_]](q"""
//     { def $name(implicit @_root_.scala.annotation.implicitNotFound($implicitMsg)
//    $param: ${appliedType(weakTypeOf[DIU#Tag[Nothing]], t)}) = $param; $name.tag }""")

}

class TagLambdaMacro(override val c: whitebox.Context {type PrefixType <: Tags#TagObject}) extends TagMacro(c) {

  import c.universe._
  import c.universe.internal.decorators._

  def lambdaImpl: c.Tree = {
    val prefixTpe = c.prefix.actualType

    if (!(prefixTpe <:< typeOf[Tags#TagObject])) {
      c.abort(c.enclosingPosition, "Tag lambda should be called only as a member of Tags#Tag companion object")
    }

    val pos = c.macroApplication.pos

    val targetTpe = c.enclosingUnit.body.collect {
      case AppliedTypeTree(t, arg :: _) if t.exists(_.pos == pos) =>
        c.typecheck(arg, c.TYPEmode, c.universe.definitions.NothingTpe, silent = false, withImplicitViewsDisabled = true, withMacrosDisabled = true)
          .tpe
    }.headOption match {
      case None =>
        c.abort(c.enclosingPosition, "Couldn't find an the type that `Tag.auto.T` macro was applied to, please make sure you use the correct syntax, as in `def tagk[F[_]: Tag.auto.T]: TagK[T] = implicitly[Tag.auto.T[F]]`")
      case Some(t) =>
        t
    }

    val kind = kindOf(targetTpe)

    logger.log(s"Found posiition $pos, target type $targetTpe, target kind $kind")

    val ctorParam = mkTypeParameter(NoSymbol, kind)
    val argStruct = mkHKTagArg(ctorParam.asType.toType, kind)

    val resultType = c.typecheck(
      tq"{ type T[${c.internal.typeDef(ctorParam)}] = $prefixTpe#HKTagRef[$argStruct] }"
      , c.TYPEmode, c.universe.definitions.NothingTpe, silent = false, withImplicitViewsDisabled = true, withMacrosDisabled = true
    ).tpe

    val res = Literal(Constant(())).setType(resultType)

    logger.log(s"final result: $resultType")

    res
  }
}

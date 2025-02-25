package izumi.distage.impl

import izumi.distage.model.definition.With
import izumi.fundamentals.platform.language.Quirks._
import izumi.fundamentals.reflection.macrortti._
import org.scalatest.WordSpec

trait YieldOpCounts {
  def zioYieldOpCount: Int = 1024
  def blockingYieldOpCount: Int = Int.MaxValue
}
object YieldOpCounts extends YieldOpCounts

class LightTypeTagTest extends WordSpec {
  trait T0[A[_], B[_]]
  final val str = "str"

  type Id[T] = T
  type FP1[+T] = List[T]
  type FP[+T] = FP1[T]
  type L[P] = List[P]
  type LN[P <: Number] = List[P]

  trait T1[U[_]]

  type FI[IGNORE] = Unit

  trait T2[U[_[_], _[_]]]

  //  type K = T1[F]
  //  val a: K = new T1[F] {}
  trait C {
    type A
  }

  trait R[K, A <: R[K, A]]

  trait R1[K] extends R[K, R1[K]]

  type S[A, B] = Either[B, A]

  trait W1

  trait W2 extends W1

  trait W3[_]

  trait W4[A] extends W3[A]

  trait I1

  trait I2 extends I1

  trait F1[+A]

  trait F2[+A] extends F1[A]

  trait FT1[+A[+ _[+ _]]]

  trait FT2[+A[+ _[+ _]]] extends FT1[A]

  trait IT1[+K[+ _]]

  trait IT2[+K[+ _]] extends IT1[K]

  trait FM1[+A, +B]

  trait FM2[+A] extends FM1[A, Unit]

  type NestedTL[G[_, _], A, B] = FM2[G[A, (B, A)]]

  type NestedTL2[A, B, G[_]] = FM2[G[S[B, A]]]

  type Const[A, B] = B

  trait H1
  trait H2 extends H1
  trait H3 extends H2
  trait H4 extends H3
  trait H5 extends H4

  trait J1[F[_]]
  trait J2
  trait J3
  trait J[F[_]] extends J1[F] with J2 with J3

  object TPrefix {
    type T
  }

  trait RoleParent[F[_]]
  trait RoleChild[F[_, _]] extends RoleParent[F[Throwable, ?]]

  def println(o: Any): Unit = info(o.toString)

  def println(o: LightTypeTag): Unit = info(o.ref.toString)

  def assertRepr(t: LightTypeTag, expected: String): Unit = {
    assert(t.toString == expected).discard()
  }

  def assertSame(t: LightTypeTag, expected: LightTypeTag): Unit = {
    val clue = s"$t =?= $expected"
    info(clue)
    assert(t =:= expected, clue).discard()
  }

  def assertDifferent(t: LightTypeTag, expected: LightTypeTag): Unit = {
    val clue = s"$t =!= $expected"
    info(clue)
    assert(!(t =:= expected), clue).discard()
  }

  def assertChild(child: LightTypeTag, parent: LightTypeTag): Unit = {
    val clue = s"$child <?< $parent"
    info(clue)
    assert(child <:< parent, clue).discard()
  }

  def assertNotChild(child: LightTypeTag, parent: LightTypeTag): Unit = {
    val clue = s"$child <!< $parent"
    info(clue)
    assert(!(child <:< parent), clue).discard()
  }


  def assertCombine(outer: LightTypeTag, inner: Seq[LightTypeTag], expected: LightTypeTag): Unit = {
    val combined = outer.combine(inner: _*)
    val clue = s"($outer)•(${inner.mkString(",")}) => $combined =?= $expected"
    info(clue)
    assert(combined =:= expected, clue).discard()
  }

  def assertCombine(outer: LightTypeTag, inner: LightTypeTag, expected: LightTypeTag): Unit = {
    val combined = outer.combine(inner)
    val clue = s"($outer)•($inner) => $combined =?= $expected"
    info(clue)
    assert(combined =:= expected, clue).discard()
  }

  def assertCombineNonPos(outer: LightTypeTag, inner: Seq[Option[LightTypeTag]], expected: LightTypeTag): Unit = {
    val combined = outer.combineNonPos(inner: _*)
    info(s"($outer)•(${inner.mkString(",")}) => $combined =?= $expected")
    assert(combined =:= expected)
    ()
  }

  def literalLtt(s: String)(implicit l: LTag[s.type]): LightTypeTag = l.tag

  "lightweight type tags" should {
    "support human-readable representation" in {
      assertRepr(LTT[Int {def a(k: String): Int; val b: String; type M1 = W1; type M2 <: W2; type M3[A] = Either[Unit, A]}],
        "({Int} & {def a(String): Int, def b(): String, type M1 = LightTypeTagTest::W1, type M2 = M2|<Nothing..LightTypeTagTest::W2>, type M3 = λ %0 → Either[+Unit,+0]})"
      )
      assertRepr(LTT[I1 with (I1 with (I1 with W1))], "{LightTypeTagTest::I1 & LightTypeTagTest::W1}")
      assertRepr(`LTT[_]`[R1], "λ %0 → LightTypeTagTest::R1[=0]")
      assertRepr(`LTT[_]`[Nothing], "Nothing")
      assertRepr(LTT[Int], "Int")
      assertRepr(LTT[List[Int]], "List[+Int]")
      assertRepr(LTT[Id[Int]], "Int")
      assertRepr(LTT[FP[Int]], "List[+Int]")
      assertRepr(`LTT[_]`[L], "λ %0 → List[+0]")
      assertRepr(`LTT[_]`[Either[Unit, ?]], "λ %0 → Either[+Unit,+0]")
      assertRepr(`LTT[_]`[S[Unit, ?]], "λ %0 → Either[+0,+Unit]")
    }

    "support typetag combination" in {
      assertCombine(`LTT[_[_]]`[T1], `LTT[_]`[Id], LTT[T1[Id]])
      assertCombine(`LTT[_[_]]`[T1], `LTT[_]`[FP], LTT[T1[FP]])
      assertCombine(`LTT[_[_]]`[T1], `LTT[_]`[FI], LTT[T1[FI]])

      assertCombine(`LTT[_[_]]`[T0[Id, ?[_]]], `LTT[_]`[FP], LTT[T0[Id, FP]])
      assertCombine(`LTT[_[_]]`[T1], `LTT[_]`[List], LTT[T1[List]])
      assertCombine(`LTT[_]`[List], LTT[Int], LTT[List[Int]])
      assertCombine(`LTT[_,_]`[Either], LTT[Unit], `LTT[_]`[Either[Unit, ?]])

      assertCombine(`LTT[_[_[_],_[_]]]`[T2], `LTT[_[_],_[_]]`[T0], LTT[T2[T0]])

      type ComplexRef[T] = W1 with T {def a(p: T): T; type M = T}
      assertCombine(`LTT[_]`[ComplexRef], LTT[Int], LTT[W1 with Int {def a(p: Int): Int; type M = Int}])
    }

    "support non-positional typetag combination" in {
      assertCombineNonPos(`LTT[_,_]`[Either], Seq(None, Some(LTT[Unit])), `LTT[_]`[Either[?, Unit]])
    }

    "support subtype checks" in {
      assertChild(LTT[Int], LTT[AnyVal])
      assertChild(LTT[Int], LTT[Int])
      assertChild(LTT[List[Int]], LTT[List[Int]])
      assertChild(LTT[List[I2]], LTT[List[I1]])
      assertChild(LTT[Either[Nothing, Int]], LTT[Either[Throwable, Int]])

      assertChild(LTT[F2[I2]], LTT[F1[I1]])
      assertChild(LTT[FT2[IT2]], LTT[FT1[IT1]])
      assertChild(`LTT[_[_[_]]]`[FT2].combine(`LTT[_[_]]`[IT2]), LTT[FT1[IT1]])

      assertChild(LTT[FT2[IT2]], LTT[FT1[IT2]])

      assertChild(LTT[List[Int]], `LTT[_]`[List])
      assertNotChild(LTT[Set[Int]], `LTT[_]`[Set])

      assertChild(LTT[FM2[I2]], LTT[FM1[I1, Unit]])
      assertChild(LTT[FM2[I2]], `LTT[_,_]`[FM1])
      assertChild(LTT[Option[Nothing]], LTT[Option[Int]])
      assertChild(LTT[None.type], LTT[Option[Int]])

      assertChild(LTT[Option[W2]], LTT[Option[_ <: W1]])
      assertNotChild(LTT[Option[W2]], LTT[Option[_ <: I1]])

      assertChild(LTT[Option[H3]], LTT[Option[_ >: H4 <: H2]])
      assertNotChild(LTT[Option[H1]], LTT[Option[_ >: H4 <: H2]])

      // bottom boundary is weird!
      assertChild(LTT[Option[H5]], LTT[Option[_ >: H4 <: H2]])

      // I consider this stuff practically useless
      type X[A >: H4 <: H2] = Option[A]
      assertNotChild(LTT[Option[H5]], `LTT[A,B,_>:B<:A]`[H2, H4, X])
      // allTypeReferences: we need to use tpe.etaExpand but 2.13 has a bug: https://github.com/scala/bug/issues/11673#
      //assertChild(LTT[Option[H3]], `LTT[A,B,_>:B<:A]`[H2, H4, X])
    }

    "support additional mixin traits after first trait with a HKT parameter" in {
      assertChild(LTT[J[Option]], LTT[J1[Option]])
      assertChild(LTT[J[Option]], LTT[J3])
      assertChild(LTT[J[Option]], LTT[J2])
      assertChild(LTT[J[Option]], LTT[J1[Option] with J2])
      assertChild(LTT[J[Option]], LTT[J2 with J3])
      assertChild(LTT[J[Option]], LTT[J1[Option] with J2 with J3])
    }

    "support swapped parents" in {
      trait KT1[+A1, +B1]
      trait KT2[+A2, +B2] extends KT1[B2, A2]

      assertChild(LTT[KT2[H1, I1]], LTT[KT1[I1, H1]])
      assertNotChild(LTT[KT2[H1, I1]], LTT[KT1[H1, I1]])

      assertChild(LTT[KT2[H2, I2]], LTT[KT1[I1, H1]])
      assertNotChild(LTT[KT2[H2, I2]], LTT[KT1[H1, I1]])

      trait KK1[+A, +B, +U]
      trait KK2[+A, +B] extends KK1[B, A, Unit]

      assertChild(LTT[KK2[Int, String]], LTT[KK1[String, Int, Unit]])

      assertChild(LTT[KK2[H2, I2]], LTT[KK1[I1, H1, Unit]])
      assertNotChild(LTT[KK2[H2, I2]], LTT[KK1[H1, I1, Unit]])

      assertChild(`LTT[_]`[KK2[H2, ?]], `LTT[_]`[KK1[?, H1, Unit]])
      assertNotChild(`LTT[_]`[KK2[H2, ?]], `LTT[_]`[KK1[H1, ?, Unit]])
    }

    "support PDTs" in {
      val a = new C {
        override type A = Int
      }

      assertSame(LTT[a.A], LTT[Int])

      val a1: C = new C {
        override type A = Int
      }
      val a2: C = new C {
        override type A = String
      }

      assertDifferent(LTT[a1.A], LTT[Int])
      assertDifferent(LTT[a1.A], LTT[a2.A])
    }

    "support subtyping of parents parameterized with type lambdas" in {
      implicitly[RoleChild[Either] <:< RoleParent[Either[Throwable, ?]]]
      assertChild(LTT[RoleChild[Either]], LTT[RoleParent[Either[Throwable, ?]]])
    }

    "support complex type lambdas" in {
      assertSame(`LTT[_,_]`[NestedTL[Const, ?, ?]], `LTT[_,_]`[Lambda[(A, B) => FM2[(B, A)]]])
      assertSame(`LTT[_[_]]`[NestedTL2[W1, W2, ?[_]]], `LTT[_[_]]`[Lambda[G[_] => FM2[G[S[W2, W1]]]]])
      assertChild(`LTT[_,_]`[NestedTL[Const, ?, ?]], `LTT[_,_]`[Lambda[(A, B) => FM2[(B, A)]]])
    }

    "support TagK* family summoners" in {
      assertSame(LTagK[List].tag, `LTT[_]`[List])
    }

    "support intersection type equality" in {
      type T1[A] = W3[A] with W1
      type T2[A] = W4[A] with W2

      assertSame(`LTT[_]`[T1], `LTT[_]`[T1])
      assertDifferent(`LTT[_]`[T1], `LTT[_]`[T2])
    }

    "support intersection type subtype checks" in {
      type F1 = W3[Int] with W1
      type F2 = W4[Int] with W2

      type T1[A] = W3[A] with W1
      type T2[A] = W4[A] with W2

      assertChild(LTT[F1], LTT[W3[Int]])
      assertChild(LTT[F1], LTT[W1])
      assertChild(LTT[F2], LTT[F1])

      assertChild(`LTT[_]`[W4], `LTT[_]`[W3])
      assertChild(`LTT[_]`[T1], `LTT[_]`[W3])
      assertChild(`LTT[_]`[T1], LTT[W1])
      assertChild(`LTT[_]`[T2], `LTT[_]`[T1])
    }

    "support structural & refinement type equality" in {
      assertDifferent(LTT[With[str.type] with ({ type T = str.type with Int })], LTT[With[str.type] with ({ type T = str.type with Long })])

      type C1 = C
      assertSame(LTT[ {def a: Int}], LTT[ {def a: Int}])
      assertSame(LTT[C {def a: Int}], LTT[C1 {def a: Int}])

      assertDifferent(LTT[C {def a: Int}], LTT[ {def a: Int}])
      assertDifferent(LTT[C {def a: Int}], LTT[C])

      assertDifferent(LTT[C {def a: Int}], LTT[C {def a: Int; def b: Int}])

      val a1 = new C {
        override type A = Int
      }
      object Z {
        type X <: {type A = Int}
      }
      Z.discard()

      assertSame(LTT[a1.A], LTT[Z.X#A])
    }

    "support structural & refinement type subtype checks" in {
      type C1 = C
      assertChild(LTT[ {def a: Int}], LTT[ {def a: Int}])
      assertChild(LTT[C {def a: Int}], LTT[C1 {def a: Int}])


      assertChild(LTT[C {def a: Int}], LTT[C])
      assertNotChild(LTT[C], LTT[C {def a: Int}])

      assertChild(LTT[C {def a: Int; def b: Int}], LTT[C {def a: Int}])
      assertNotChild(LTT[C {def a: Int}], LTT[C {def a: Int; def b: Int}])

      assertChild(LTT[C {def a: Int}], LTT[ {def a: Int}])
    }

    "support literal types" in {
      assertRepr(literalLtt("str"), "\"str\"")
      assertSame(literalLtt("str2"), literalLtt("str2"))
      assertDifferent(literalLtt("str1"), literalLtt("str2"))

      assertChild(literalLtt("str"), LTT[String])
      assertNotChild(literalLtt("str"), LTT[Int])
      assertNotChild(LTT[String], literalLtt("str"))
      assertDifferent(LTT[String], literalLtt("str"))
    }

    "resolve concrete types through PDTs and projections" in {
      val a1 = new C {
        override type A <: Int
      }
      object Z {
        type X <: {type A = Int}
      }
      Z.discard()

      assertChild(LTT[a1.A], LTT[Z.X#A])
      assertNotChild(LTT[Z.X#A], LTT[a1.A])
    }

    "resolve comparisons of object and trait with the same name" in {
      assertNotChild(LTT[YieldOpCounts.type], LTT[RoleChild[Either]])
      assertChild(LTT[YieldOpCounts.type], LTT[YieldOpCounts])
      assertDifferent(LTT[YieldOpCounts.type], LTT[YieldOpCounts])
      assertNotChild(LTT[YieldOpCounts], LTT[YieldOpCounts.type])
    }

    "strong summons test" in {
      assertTypeError("def x1[T] = LTag[Array[T]]")
      assertTypeError("def x1[T] = LTag[Array[Int] { type X = T }]")
      assertTypeError("def x1[T <: { type Array }] = LTag[T#Array]")
      assertTypeError("def x1[T] = LTag[Array[Int] with List[T]]")
      assertTypeError("def x1[F[_]] = LTag[F[Int]]")

      assertCompiles("def x1 = { object x { type T }; def x1 = LTag[Array[x.T]].discard() }")
      assertCompiles("def x1 = { object x { type T }; LTag[Array[Int] { type X = x.T }].discard() }")
      assertCompiles("def x1 = { object x { type T <: { type Array } }; LTag[x.T#Array].discard() }")
      assertCompiles("def x1 = { object x { type T }; LTag[Array[Int] with List[x.T]].discard() }")
      assertCompiles("def x1 = { object x { type F[_] }; LTag[x.F[Int]].discard() }")
    }

    "resolve prefixes of annotated types" in {
      assert(LTT[TPrefix.T @unchecked] == LTT[TPrefix.T])
    }

  }
}

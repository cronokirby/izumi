package izumi.fundamentals.reflection.macrortti

import java.nio.ByteBuffer

import boopickle.{DefaultBasic, Pickler}
import izumi.fundamentals.reflection.TrivialMacroLogger
import izumi.fundamentals.reflection.macrortti.LightTypeTagRef.{AbstractReference, AppliedReference, FullReference, NameReference, TypeParam}

abstract class LightTypeTag
(
  bases: () => Map[AbstractReference, Set[AbstractReference]],
  db: () => Map[NameReference, Set[NameReference]],
) extends Serializable {

  val ref: LightTypeTagRef
  protected[macrortti] lazy val basesdb: Map[AbstractReference, Set[AbstractReference]] = bases()
  protected[macrortti] lazy val idb: Map[NameReference, Set[NameReference]] = db()

  @inline final def <:<(maybeParent: LightTypeTag): Boolean = {
    new LightTypeTagInheritance(this, maybeParent).isChild()
  }

  @inline final def =:=(other: LightTypeTag): Boolean = {
    this == other
  }

  def combine(o: LightTypeTag*): LightTypeTag = {

    def mergedInhDb: Map[NameReference, Set[NameReference]] =
      o.foldLeft(idb) {
        case (acc, v) =>
          LightTypeTag.mergeIDBs(acc, v.idb)
      }

    def mergedBases: Map[AbstractReference, Set[AbstractReference]] = {
      o.foldLeft(basesdb) {
        case (acc, v) =>
          LightTypeTag.mergeIDBs(acc, v.basesdb)
      }
    }

    LightTypeTag(ref.combine(o.map(_.ref)), mergedBases, mergedInhDb)
  }

  def combineNonPos(o: Option[LightTypeTag]*): LightTypeTag = {

    def mergedInhDb: Map[NameReference, Set[NameReference]] = {
      o.foldLeft(idb) {
        case (acc, v) =>
          LightTypeTag.mergeIDBs(acc, v.map(_.idb).getOrElse(Map.empty))
      }
    }

    def mergedBases: Map[AbstractReference, Set[AbstractReference]] = {
      o.foldLeft(basesdb) {
        case (acc, v) =>
          LightTypeTag.mergeIDBs(acc, v.map(_.basesdb).getOrElse(Map.empty))
      }
    }

    LightTypeTag(ref.combineNonPos(o.map(_.map(_.ref))), mergedBases, mergedInhDb)
  }

  override def toString: String = {
    //    import izumi.fundamentals.reflection.macrortti.LTTRenderables.Long._
    //    t.render()
    ref.toString
  }

  def repr: String = {
    import izumi.fundamentals.reflection.macrortti.LTTRenderables.Long._
    ref.render()
  }

  override def equals(other: Any): Boolean = {
    other match {
      case that: LightTypeTag =>
        ref == that.ref
      case _ => false
    }
  }

  override def hashCode(): Int = {
    val state = Seq(ref)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object LightTypeTag {
  @inline def apply(ref0: LightTypeTagRef, bases: => Map[AbstractReference, Set[AbstractReference]], db: => Map[NameReference, Set[NameReference]]): LightTypeTag = {
    new LightTypeTag(() => bases, () => db) {
      override final val ref: LightTypeTagRef = ref0
    }
  }

  final class ParsedLightTypeTag(
                                  private val refString: String,
                                  bases: () => Map[AbstractReference, Set[AbstractReference]],
                                  db: () => Map[NameReference, Set[NameReference]],
                                ) extends LightTypeTag(bases, db) {
    override lazy val ref: LightTypeTagRef = {
      binary0Serializer.unpickle(ByteBuffer.wrap(refString.getBytes("ISO-8859-1")))
    }

    override def equals(other: Any): Boolean = {
      other match {
        case that: ParsedLightTypeTag if refString == that.refString =>
          true
        case _ =>
          super.equals(other)
      }
    }
  }

  // strict parse entire ltt
  def parse[T](s: String): LightTypeTag = {
    val bytes = s.getBytes("ISO-8859-1")
    binarySerializer.unpickle(ByteBuffer.wrap(bytes, 0, bytes.length))
  }

  // parse lazy ParsedLightTypeTag
  def parse[T](s1: String, s2: String): LightTypeTag = {
    lazy val shared = {
      binary1Serializer.unpickle(ByteBuffer.wrap(s2.getBytes("ISO-8859-1")))
    }

    new ParsedLightTypeTag(s1, () => shared.bases, () => shared.idb)
  }

  final case class Shared(bases: Map[AbstractReference, Set[AbstractReference]], idb: Map[NameReference, Set[NameReference]])

  val (binarySerializer: Pickler[LightTypeTag], binary0Serializer: Pickler[LightTypeTagRef], binary1Serializer: Pickler[Shared]) = {
    import boopickle.Default._
    implicit val serializer6 = generatePickler[AppliedReference]
    implicit val serializer4 = generatePickler[NameReference]
    implicit val serializer5 = generatePickler[AbstractReference]

    implicit val refSerializer = generatePickler[LightTypeTagRef]
    implicit val z = generatePickler[Shared]

    val fltt = DefaultBasic.transformPickler[LightTypeTag,
      (
        LightTypeTagRef,
        Map[AbstractReference, Set[AbstractReference]],
        Map[NameReference, Set[NameReference]],
      )
    ] {
      case (a, b, c) => apply(a, b, c)
    } {
      l => (l.ref, l.basesdb, l.idb)
    }

    (fltt, refSerializer, z)
  }

  final val loggerId = TrivialMacroLogger.id("rtti")

  object ReflectionLock

  private[macrortti] def mergeIDBs[T](self: Map[T, Set[T]], other: Map[T, Set[T]]): Map[T, Set[T]] = {
    import izumi.fundamentals.collections.IzCollections._

    val both = self.toSeq ++ other.toSeq
    both.toMultimap.mapValues(_.flatten).toMap
  }

}

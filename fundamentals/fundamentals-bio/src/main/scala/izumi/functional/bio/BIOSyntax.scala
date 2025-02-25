package izumi.functional.bio

import izumi.functional.bio.BIOSyntax.BIOImplicitPuns

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.language.implicitConversions

trait BIOSyntax extends BIOImplicitPuns {
  /**
    * A convenient dependent summoner for BIO* hierarchy.
    * Auto-narrows to the most powerful available class:
    *
    * {{{
    *   def y[F[+_, +_]: BIOAsync] = {
    *     F.timeout(5.seconds)(F.forever(F.unit))
    *   }
    * }}}
    *
    */
  def F[F[+_, +_]](implicit F: BIOFunctor[F]): F.type = F
}

object BIOSyntax {

  class BIOFunctorOps[F[_, + _], E, A](protected[this] val r: F[E, A])(implicit protected[this] val F: BIOFunctor[F]) {
    @inline final def map[B](f: A => B): F[E, B] = F.map(r)(f)

    @inline final def as[B](b: => B): F[E, B] = F.map(r)(_ => b)
    @inline final def void: F[E, Unit] = F.void(r)
    @inline final def widen[A1](implicit @deprecated("unused","") ev: A <:< A1): F[E, A1] = r.asInstanceOf[F[E, A1]]
  }

  class BIOBifunctorOps[F[+_, +_], E, A](override protected[this] val r: F[E, A])(implicit override protected[this] val F: BIOBifunctor[F]) extends BIOFunctorOps(r) {
    @inline final def leftMap[E2](f: E => E2): F[E2, A] = F.leftMap(r)(f)
    @inline final def bimap[E2, B](f: E => E2, g: A => B): F[E2, B] = F.bimap(r)(f, g)

    @inline final def widenError[E1](implicit @deprecated("unused","") ev: E <:< E1): F[E1, A] = r.asInstanceOf[F[E1, A]]
  }

  class BIOApplicativeOps[F[+_, +_], E, A](override protected[this] val r: F[E, A])(implicit override protected[this] val F: BIOApplicative[F]) extends BIOBifunctorOps(r) {

    /** execute two operations in order, return result of second operation */
    @inline final def *>[E1 >: E, B](f0: => F[E1, B]): F[E1, B] = F.*>[E, A, E1, B](r, f0)

    /** execute two operations in order, same as `*>`, but return result of first operation */
    @inline final def <*[E1 >: E, B](f0: => F[E1, B]): F[E1, A] = F.<*[E, A, E1, B](r, f0)

    /** execute two operations in order, return result of both operations */
    @inline final def zip[E2 >: E, B, C](r2: => F[E2, B]): F[E2, (A, B)] = F.map2(r, r2)(_ -> _)

    /** execute two operations in order, map their results */
    @inline final def map2[E2 >: E, B, C](r2: => F[E2, B])(f: (A, B) => C): F[E2, C] = F.map2(r, r2)(f)

    @inline final def forever: F[E, Nothing] = F.forever(r)
  }

  class BIOGuaranteeOps[F[+_, +_], E, A](override protected[this] val r: F[E, A])(implicit override protected[this] val F: BIOGuarantee[F]) extends BIOApplicativeOps(r) {
    @inline final def guarantee(cleanup: F[Nothing, Unit]): F[E, A] = F.guarantee(r)(cleanup)
  }

  final class BIOMonadOps[F[+_, +_], E, A](override protected[this] val r: F[E, A])(implicit override protected[this] val F: BIOMonad[F]) extends BIOApplicativeOps(r) {
    @inline final def flatMap[E1 >: E, B](f0: A => F[E1, B]): F[E1, B] = F.flatMap[E, A, E1, B](r)(f0)
    @inline final def tap[E1 >: E, B](f0: A => F[E1, Unit]): F[E1, A] = F.flatMap[E, A, E1, A](r)(a => F.map(f0(a))(_ => a))

    @inline final def flatten[E1 >: E, A1](implicit ev: A <:< F[E1, A1]): F[E1, A1] = F.flatten(r.widen)
  }

  class BIOErrorOps[F[+_, +_], E, A](override protected[this] val r: F[E, A])(implicit override protected[this] val F: BIOError[F]) extends BIOGuaranteeOps(r) {
    @inline final def catchAll[E2, A2 >: A](h: E => F[E2, A2]): F[E2, A2] = F.catchAll[E, A, E2, A2](r)(h)
    @inline final def catchSome[E2 >: E, A2 >: A](h: PartialFunction[E, F[E2, A2]]): F[E2, A2] = F.catchSome[E, A, E2, A2](r)(h)

    @inline final def redeem[E2, B](err: E => F[E2, B], succ: A => F[E2, B]): F[E2, B] = F.redeem[E, A, E2, B](r)(err, succ)
    @inline final def redeemPure[B](err: E => B, succ: A => B): F[Nothing, B] = F.redeemPure(r)(err, succ)

    @inline final def tapError[E1 >: E](f: E => F[E1, Unit]): F[E1, A] = F.tapError[E, A, E1](r)(f)

    @inline final def attempt: F[Nothing, Either[E, A]] = F.attempt(r)

    @inline final def flip: F[A, E] = F.flip(r)
  }

  class BIOMonadErrorOps[F[+_, +_], E, A](override protected[this] val r: F[E, A])(implicit override protected[this] val F: BIOMonadError[F]) extends BIOErrorOps(r) {
    @inline final def flatMap[E1 >: E, B](f0: A => F[E1, B]): F[E1, B] = F.flatMap[E, A, E1, B](r)(f0)
    @inline final def tap[E1 >: E, B](f0: A => F[E1, Unit]): F[E1, A] = F.flatMap[E, A, E1, A](r)(a => F.map(f0(a))(_ => a))

    @inline final def flatten[E1 >: E, A1](implicit ev: A <:< F[E1, A1]): F[E1, A1] = F.flatten(r.widen)

    @inline final def leftFlatMap[E2](f: E => F[Nothing, E2]): F[E2, A] = F.leftFlatMap(r)(f)

    @inline final def tapBoth[E1 >: E, E2 >: E1](err: E => F[E1, Unit])(succ: A => F[E2, Unit]): F[E2, A] = F.tapBoth[E, A, E2](r)(err, succ)

    @inline final def fromEither[E1 >: E, A1](implicit ev: A <:< Either[E1, A1]): F[E1, A1] = F.flatMap[E, A, E1, A1](r)(F.fromEither[E1, A1](_))
    @inline final def fromOption[E1 >: E, A1](errorOnNone: => E1)(implicit ev1: A <:< Option[A1]): F[E1, A1] = F.flatMap[E, A, E1, A1](r)(F.fromOption(errorOnNone)(_))
  }

  class BIOBracketOps[F[+_, +_], E, A](override protected[this] val r: F[E, A])(implicit override protected[this] val F: BIOBracket[F]) extends BIOMonadErrorOps(r) {
    @inline final def bracket[E1 >: E, B](release: A => F[Nothing, Unit])(use: A => F[E1, B]): F[E1, B] =
      F.bracket(r: F[E1, A])(release)(use)
  }

  class BIOPanicOps[F[+_, +_], E, A](override protected[this] val r: F[E, A])(implicit override protected[this] val F: BIOPanic[F]) extends BIOBracketOps(r) {
    @inline final def sandbox: F[BIOExit.Failure[E], A] = F.sandbox(r)
    @inline final def sandboxBIOExit: F[Nothing, BIOExit[E, A]] = F.redeemPure(F.sandbox(r))(identity, BIOExit.Success(_))

    /**
      * Catch all _defects_ in this effect and convert them to Throwable
      * Example:
      *
      * {{{
      *   BIO[F].pure(1)
      *     .map(_ => ???)
      *     .sandboxThrowable
      *     .catchAll(_ => BIO(println("Caught error!")))
      * }}}
      *
      */
    @inline final def sandboxToThrowable(implicit ev: E <:< Throwable): F[Throwable, A] =
      F.catchAll(F.sandbox(r))(failure => F.fail(failure.toThrowable))

    /** Convert Throwable typed error into a defect */
    @inline final def orTerminate(implicit ev: E <:< Throwable): F[Nothing, A] = F.catchAll(r)(F.terminate(_))
  }

  class BIOOps[F[+_, +_], E, A](override protected[this] val r: F[E, A])(implicit override protected[this] val F: BIO[F]) extends BIOPanicOps(r) {
    @inline final def bracketAuto[E1 >: E, B](use: A => F[E1, B])(implicit ev: A <:< AutoCloseable): F[E1, B] =
      F.bracket[E1, A, B](r)(c => F.sync(c.close()))(use)
  }

  final class BIOAsyncOps[F[+_, +_], E, A](override protected[this] val r: F[E, A])(implicit override protected[this] val F: BIOAsync[F]) extends BIOOps(r) {
    @inline final def retryOrElse[A2 >: A, E2](duration: FiniteDuration, orElse: => F[E2, A2]): F[E2, A2] = F.retryOrElse[A, E, A2, E2](r)(duration, orElse)
    @inline final def repeatUntil[E2 >: E, A2](onTimeout: => E2, sleep: FiniteDuration, maxAttempts: Int)(implicit ev: A <:< Option[A2]): F[E2, A2] =
      F.repeatUntil[E2, A2](new BIOFunctorOps(r)(F).widen)(onTimeout, sleep, maxAttempts)

    @inline final def timeout(duration: Duration): F[E, Option[A]] = F.timeout(r)(duration)
    @inline final def timeoutFail[E1 >: E](e: E1)(duration: Duration): F[E1, A] =
      F.flatMap(timeout(duration): F[E1, Option[A]])(_.fold[F[E1, A]](F.fail(e))(F.pure))

    @inline final def race[E1 >: E, A1 >: A](that: F[E1, A1]): F[E1, A1] = F.race(r, that)
  }

  final class BIOForkOps[F[+_, +_], E, A](private val r: F[E, A])(implicit private val F: BIOFork[F]) {
    @inline final def fork: F[Nothing, BIOFiber[F, E, A]] = F.fork(r)
  }

  final class BIOFork3Ops[F[-_, +_, +_], R, E, A](private val r: F[R, E, A])(implicit private val F: BIOFork3[F]) {
    @inline final def fork: F[R, Nothing, BIOFiber[F[Any, +?, +?], E, A]] = F.fork(r)
  }

  trait BIOImplicitPuns extends BIOImplicitPuns1 {
    @inline implicit final def BIOAsync[F[+_, +_]: BIOAsync, E, A](self: F[E, A]): BIOSyntax.BIOAsyncOps[F, E, A] = new BIOSyntax.BIOAsyncOps[F, E, A](self)
    @inline final def BIOAsync[F[+_, +_]: BIOAsync]: BIOAsync[F] = implicitly

    @inline implicit final def BIOFork[F[+_, +_]: BIOFork, E, A](self: F[E, A]): BIOSyntax.BIOForkOps[F, E, A] = new BIOSyntax.BIOForkOps[F, E, A](self)
    @inline final def BIOFork[F[+_, +_]: BIOFork]: BIOFork[F] = implicitly

    @inline implicit final def BIOFork3[F[-_, +_, +_]: BIOFork3, R, E, A](self: F[R, E, A]): BIOSyntax.BIOFork3Ops[F, R, E, A] = new BIOSyntax.BIOFork3Ops[F, R, E, A](self)
    @inline final def BIOFork3[F[-_, +_, +_]: BIOFork3]: BIOFork3[F] = implicitly

    @inline final def BIOPrimitives[F[+_, +_]: BIOPrimitives]: BIOPrimitives[F] = implicitly
  }
  trait BIOImplicitPuns1 extends BIOImplicitPuns2 {
    @inline implicit final def BIO[F[+_, +_]: BIO, E, A](self: F[E, A]): BIOSyntax.BIOOps[F, E, A] = new BIOSyntax.BIOOps[F, E, A](self)
    /**
     * Shorthand for [[BIO#syncThrowable]]
     *
     * {{{
     *   BIO(println("Hello world!"))
     * }}}
     * */
    @inline final def BIO[F[+_, +_], A](effect: => A)(implicit F: BIO[F]): F[Throwable, A] = F.syncThrowable(effect)
    @inline final def BIO[F[+_, +_]: BIO]: BIO[F] = implicitly
  }
  trait BIOImplicitPuns2 extends BIOImplicitPuns3 {
    @inline implicit final def BIOPanic[F[+_, +_]: BIOPanic, E, A](self: F[E, A]): BIOSyntax.BIOPanicOps[F, E, A] = new BIOSyntax.BIOPanicOps[F, E, A](self)
    @inline final def BIOPanic[F[+_, +_]: BIOPanic]: BIOPanic[F] = implicitly
  }
  trait BIOImplicitPuns3 extends BIOImplicitPuns4 {
    @inline implicit final def BIOBracket[F[+_, +_]: BIOBracket, E, A](self: F[E, A]): BIOSyntax.BIOBracketOps[F, E, A] = new BIOSyntax.BIOBracketOps[F, E, A](self)
    @inline final def BIOBracket[F[+_, +_]: BIOBracket]: BIOBracket[F] = implicitly
  }
  trait BIOImplicitPuns4 extends BIOImplicitPuns5 {
    @inline implicit final def BIOMonadError[F[+_, +_]: BIOMonadError, E, A](self: F[E, A]): BIOSyntax.BIOMonadErrorOps[F, E, A] = new BIOSyntax.BIOMonadErrorOps[F, E, A](self)
    @inline final def BIOMonadError[F[+_, +_]: BIOMonadError]: BIOMonadError[F] = implicitly
  }
  trait BIOImplicitPuns5 extends BIOImplicitPuns6 {
    @inline implicit final def BIOError[F[+_, +_]: BIOError, E, A](self: F[E, A]): BIOSyntax.BIOErrorOps[F, E, A] = new BIOSyntax.BIOErrorOps[F, E, A](self)
    @inline final def BIOError[F[+_, +_]: BIOError]: BIOError[F] = implicitly
  }
  trait BIOImplicitPuns6 extends BIOImplicitPuns7 {
    @inline implicit final def BIOGuarantee[F[+_, +_]: BIOGuarantee, E, A](self: F[E, A]): BIOSyntax.BIOGuaranteeOps[F, E, A] = new BIOSyntax.BIOGuaranteeOps[F, E, A](self)
    @inline final def BIOGuarantee[F[+_, +_]: BIOGuarantee]: BIOGuarantee[F] = implicitly
  }
  trait BIOImplicitPuns7 extends BIOImplicitPuns8 {
    @inline implicit final def BIOMonad[F[+_, +_]: BIOMonad, E, A](self: F[E, A]): BIOSyntax.BIOMonadOps[F, E, A] = new BIOSyntax.BIOMonadOps[F, E, A](self)
    @inline final def BIOMonad[F[+_, +_]: BIOMonad]: BIOMonad[F] = implicitly
  }
  trait BIOImplicitPuns8 extends BIOImplicitPuns9 {
    @inline implicit final def BIOApplicative[F[+_, +_]: BIOApplicative, E, A](self: F[E, A]): BIOSyntax.BIOApplicativeOps[F, E, A] = new BIOSyntax.BIOApplicativeOps[F, E, A](self)
    @inline final def BIOApplicative[F[+_, +_]: BIOApplicative]: BIOApplicative[F] = implicitly
  }
  trait BIOImplicitPuns9 extends BIOImplicitPuns10 {
    @inline implicit final def BIOBifunctor[F[+_, +_]: BIOBifunctor, E, A](self: F[E, A]): BIOSyntax.BIOBifunctorOps[F, E, A] = new BIOSyntax.BIOBifunctorOps[F, E, A](self)
    @inline final def BIOBifunctor[F[+_, +_]: BIOBifunctor]: BIOBifunctor[F] = implicitly
  }
  trait BIOImplicitPuns10 {
    @inline implicit final def BIOFunctor[F[_, + _] : BIOFunctor, E, A](self: F[E, A]): BIOSyntax.BIOFunctorOps[F, E, A] = new BIOSyntax.BIOFunctorOps[F, E, A](self)
    @inline final def BIOFunctor[F[_, +_]: BIOFunctor]: BIOFunctor[F] = implicitly
  }

}

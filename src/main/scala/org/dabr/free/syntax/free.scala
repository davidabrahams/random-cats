package org.dabr.free

import cats.{InjectK, ~>}
import cats.free.{Free, FreeApplicative => FreeAp}

private object Utils {
  def freeApFunctionK[F[_], A]: F ~> FreeAp[F, *] =
    new (F ~> FreeAp[F, *]) {
      def apply[B](fa: F[B]): FreeAp[F, B] = FreeAp.lift(fa)
    }
  // We can lift any Free[F] into a Free[FreeAp[F, ?]]
  def liftAp[F[_], A](free: Free[F, A]): Free[FreeAp[F, ?], A] = free.mapK(freeApFunctionK)
}

trait FreeSyntax {
  implicit final def syntaxFreeOps[F[_], A](free: Free[F, A]): FreeOps[F, A] =
    new FreeOps[F, A](free)
  implicit final def syntaxFreeSOps[F[_], A](free: Free[FreeAp[F, *], A]): FreeSOps[F, A] =
    new FreeSOps[F, A](free)
  implicit final def syntaxFreeApCompanionOps(free: FreeAp.type): FreeApCompanionOps =
    new FreeApCompanionOps(free)
}

final class FreeOps[F[_], A](private val free: Free[F, A]) extends AnyVal {

  /**
   * Similar to liftInject, lets us flatMap without explicitly injecting the returned value into
   * the coproduct type
   */
  def flatMapInject[G[_], B](f: A => G[B])(implicit inj: InjectK[G, F]): Free[F, B] =
    free.flatMap { a =>
      Free.liftF(inj.inj(f(a)))
    }
}

/**
 * https://typelevel.org/cats/guidelines.html#partially-applied-type-params%20Partially%20Applied%20Type%20Params%20technique
 */
final private[free] class FreeApInjectKPartiallyApplied[F[_], G[_]](
    private val dummy: Boolean = true
) extends AnyVal {
  def apply[A](fa: F[A])(implicit inj: InjectK[F, G]): FreeAp[G, A] =
    FreeAp.lift(inj.inj(fa))
}

final class FreeApCompanionOps(private val free: FreeAp.type) extends AnyVal {

  /**
   * I'm not sure why the cats companion doesn't define inject
   */
  def inject[F[_], G[_]] = new FreeApInjectKPartiallyApplied[F, G]
}

final class FreeSOps[F[_], A](private val free: Free[FreeAp[F, *], A]) extends AnyVal {
  // Perform a block of parallel steps
  def andThenPar[B](f: A => FreeAp[F, B]): Free[FreeAp[F, *], B] =
    free.flatMap { a =>
      Free.liftF(f(a))
    }

  // Perform a block of sequential steps
  def andThenSeq[B](f: A => Free[F, B]): Free[FreeAp[F, *], B] =
    free.flatMap { a =>
      Utils.liftAp(f(a))
    }

  // Perform a block of parallel steps
  // def andThenParInject[G[_], B](f: A => G[B])(implicit inj: InjectK[G, F]): Free[FreeAp[F, *], B] = {
  //   val parFn: A => FreeAp[F, B] = { a =>
  //     inj.inject()
  //   }
  //   ???
  //   }
}

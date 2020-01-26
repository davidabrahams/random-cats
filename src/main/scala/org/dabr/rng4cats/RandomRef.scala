package org.dabr.rng4cats

import cats.effect.concurrent.Ref
import cats.effect.Sync

/**
 * This is a pure, atomic, reference to a [[Random]]. This differs from cats'
 * [[cats.effect.concurrent.Ref]], as it prevents you from accidentally "reusing" an instance of
 * Random by not properly updating the mutable state.
 */
trait RandomRef[F[_]] {

  /**
   * Generate a random A, and advance our our random state. It is expected that you use map,
   * flatMap, and product to produce a program requiring multiple random numbers. This is meant to
   * interop with the functions defined on the [[Random]] companion, eg [[Random.nextLong]].
   */
  def use[A, B](f: (Random => (Random, A))): F[A]

  /**
   * Check the current Random state. It is recommended you only use this for debugging and logging,
   * and use [[use]] when you actually need to produce random numbers.
   */
  def read: F[Random]
}

final private class RandomRefImpl[F[_]](rngRef: Ref[F, Random]) extends RandomRef[F] {
  def use[A, B](f: (Random => (Random, A))): F[A] = rngRef.modify(f)
  def read: F[Random] = rngRef.get
}

object RandomRef {
  def apply[F[_]](rng: Random)(implicit F: Sync[F]): F[RandomRef[F]] =
    F.map(Ref.of[F, Random](rng))(new RandomRefImpl(_))
}

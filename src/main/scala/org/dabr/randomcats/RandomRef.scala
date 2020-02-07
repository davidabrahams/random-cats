package org.dabr.randomcats

import cats.effect.concurrent.Ref
import cats.FlatMap

/**
 * This is a pure, atomic, reference to a [[Random]]. This differs from cats'
 * [[cats.effect.concurrent.Ref]], as it prevents you from accidentally "reusing" an instance of
 * Random by not properly updating the mutable state.
 */
trait RandomRef[F[_]] {

  def next[A](f: (Random => (Random, A))): F[A]

  /**
   * Consume a random A, and advance our our random state. This is meant to
   * interop with the functions defined on the [[Random]] companion, eg [[Random.nextLong]].
   */
  def use[A, B](f: (Random => (Random, A)))(g: A => B): F[B]

  /**
   * Consume a random A, advance our our random state, and sequentially compose an effect with
   * FlatMap
   */
  def useF[A, B](f: (Random => (Random, A)))(g: A => F[B])(implicit F: FlatMap[F]): F[B]

  /**
   * Check the current Random state. It is recommended you only use this for debugging and logging,
   * and use [[use]] when you actually need to produce random numbers.
   */
  def read: F[Random]
}

final private class RandomRefImpl[F[_]](rngRef: Ref[F, Random]) extends RandomRef[F] {
  def next[A](f: (Random => (Random, A))): F[A] = rngRef.modify(f)
  def use[A, B](f: (Random => (Random, A)))(g: A => B): F[B] = {
    val composed: (Random => (Random, B)) = f.andThen { case (r, a) => (r, g(a)) }
    rngRef.modify(composed)
  }
  def useF[A, B](f: (Random => (Random, A)))(g: A => F[B])(implicit F: FlatMap[F]): F[B] =
    F.flatMap(rngRef.modify(f)) { a =>
      g(a)
    }

  def read: F[Random] = rngRef.get
}

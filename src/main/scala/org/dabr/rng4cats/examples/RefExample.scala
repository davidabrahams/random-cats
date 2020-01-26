package org.dabr.rng4cats.examples

import cats.{Monad, Parallel}
import cats.effect.{IO, ContextShift}
import cats.implicits._
import cats.effect.concurrent.Ref

import scala.concurrent.ExecutionContext

import org.dabr.rng4cats.{Random, RandomImpl, RandomRef, Seed}

/**
 * This example demonstrates using RandomRef to concurrently generate random Longs, without ever
 * reusing the same seed twice.
 * We perform 9 parallel writes with 3 keys, performing 3 writes per key. We never have collisions
 * on our hashes, and our final Random state is always the same (as we have moved 9 seeds forward).
 */
object RefExample {

  /**
   * Returns true if all writes were successful
   */
  def parallelWrite[F[_]: Monad: Parallel](
      writes: List[(Key, Value)],
      store: HashedStore[F],
      rngRef: RandomRef[F]
  ): F[Boolean] = {
    val successes: F[List[Boolean]] = writes.parTraverse {
      case (k, v) =>
        rngRef.use(Random.nextLong).flatMap { l =>
          store.put(k, new Hash(l), v)
        }
    }
    successes.map { list =>
      list.forall(identity)
    }
  }

  def main(args: Array[String]): Unit = {
    implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
    val keys: List[Key] =
      List("foo", "foo", "foo", "bar", "bar", "bar", "baz", "baz", "baz").map(new Key(_))
    val values: List[Value] = List(1, 2, 3, 4, 5, 6, 7, 8, 9).map(new Value(_))
    val writes = keys.zip(values)
    val rng: Random = Random(42)
    val io: IO[Unit] = for {
      ref <- Ref.of[IO, Map[(Key, Hash), Value]](Map.empty)
      store = new RefHashedStore[IO](ref)
      randomRef <- RandomRef[IO](rng)
      writeSuccess <- parallelWrite[IO](writes, store, randomRef)
      // even though we are sharing access to a mutable Random reference, we still produce
      // entirely unique hashes
      _ <- IO(assert(writeSuccess))
      // we always end up at the same final state
      finalRng <- randomRef.read
      _ <- IO(assert(finalRng == new RandomImpl(new Seed(214509311759785L))))
    } yield ()

    io.unsafeRunSync
  }
}

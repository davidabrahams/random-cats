package org.dabr.randomcats.examples

import cats.{Monad, Parallel}
import cats.data.StateT
import cats.effect.{IO, ContextShift}
import cats.implicits._
import cats.effect.concurrent.Ref

import scala.concurrent.ExecutionContext

import org.dabr.randomcats.{Random, RandomImpl, Seed}

/**
 * This is an equivalent example of [[RefExample]], except we use [[cats.data.StateT]] to keep track
 * of our random state.
 */
object StateExample {

  /**
   * Returns true if all writes were successful
   */
  def parallelWrite[F[_]: Parallel](
      writes: List[(Key, Value)],
      store: HashedStore[F]
  )(implicit F: Monad[F]): StateT[F, Random, Boolean] = {
    val hashes: StateT[F, Random, List[Long]] =
      Random.stateT[F, List[Long]](Random.listOf(writes.length, Random.nextLong))
    val successes: StateT[F, Random, List[Boolean]] = hashes.flatMapF { longs =>
      longs.zip(writes).parTraverse {
        case (l, (k, v)) => store.put(k, Hash(l), v)
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
      rngAndWriteSuccess <- parallelWrite[IO](writes, store).run(rng)
      finalRng = rngAndWriteSuccess._1
      writeSuccess = rngAndWriteSuccess._2
      _ <- IO(assert(writeSuccess))
      _ <- IO(assert(finalRng == RandomImpl(Seed(214509311759785L))))
    } yield ()

    io.unsafeRunSync
  }
}

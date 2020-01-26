package org.dabr.rng4cats.examples

import cats.Monad
import cats.data.StateT
import cats.effect.IO
import cats.implicits._
import cats.effect.concurrent.Ref

import org.dabr.rng4cats.examples.Hash

import org.dabr.rng4cats._

object StateExample {

  /**
   * Returns true if all writes were successful
   */
  def parallelWrite[F[_]](
      writes: List[(Key, Value)],
      store: HashedStore[F]
  )(implicit F: Monad[F]): StateT[F, Random, Boolean] = {
    // TODO: there may be a one-liner which accomplished this
    val randomStateF: StateT[F, Random, Long] = StateT {
      val f: Random => (Random, Long) = Random.nextLong
      f.andThen(F.pure)
    }
    // TODO: can this be parTraverse?
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    val stateList: StateT[F, Random, List[Boolean]] = writes.traverse {
      case (k, v) =>
        randomStateF.flatMap { l =>
          StateT.liftF(store.put(k, new Hash(l), v))
        }
    }
    stateList.map { list =>
      list.forall(identity)
    }
  }

  def main(args: Array[String]): Unit = {
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
      _ <- IO(assert(finalRng == new RandomImpl(new Seed(214509311759785L))))
    } yield ()

    io.unsafeRunSync
  }
}

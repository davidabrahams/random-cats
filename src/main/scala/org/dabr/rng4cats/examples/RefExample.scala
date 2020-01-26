package org.dabr.rng4cats.examples

import scala.concurrent.ExecutionContext
import cats._
import cats.effect._
import cats.implicits._
import cats.effect.concurrent.Ref

import org.dabr.rng4cats._

final case class Key(val s: String) extends AnyVal
final case class Hash(val l: Long) extends AnyVal
final case class Value(val i: Int) extends AnyVal

/**
 * Represents a K->V storage, where we never always append a random hash to our key, to avoid
 * writing to the same location twice
 */
trait Store[F[_]] {
  // returns true if the (Key, Hash) pair is unique, and got added to the store
  def put(k: Key, hash: Hash, v: Value): F[Boolean]
  def get(k: Key): F[Option[Value]]
  def hashes(k: Key): F[List[Hash]]
}

final case class RefStore[F[_]: Monad](ref: Ref[F, Map[(Key, Hash), Value]]) extends Store[F] {
  def put(k: Key, hash: Hash, v: Value): F[Boolean] = ref.modify { map =>
    val key = (k, hash)
    map.contains(key) match {
      case true  => (map, false) // can't do the insertion
      case false => (map + (key -> v), true)
    }
  }
  def get(k: Key): F[Option[Value]] = ref.get.map { map =>
    map.keys
      .filter {
        case (k0, _) => k0 == k
      }
      .collectFirst {
        case (key, hash) if map.contains((key, hash)) => map((key, hash))
      }
  }

  def hashes(k: Key): F[List[Hash]] = ref.get.map { map =>
    map.keys.collect {
      case (k0, h) if k0 == k => h
    }.toList
  }
}

object RefExample {

  /**
   * Returns true if all writes were successful
   */
  def parallelWrite[F[_]: Monad: Parallel](
      writes: List[(Key, Value)],
      store: Store[F],
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
      store = RefStore[IO](ref)
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

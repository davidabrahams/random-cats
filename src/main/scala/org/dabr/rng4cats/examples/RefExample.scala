package org.dabr.rng4cats.examples

import scala.concurrent.ExecutionContext
import cats._
import cats.effect._
import cats.implicits._
import cats.effect.concurrent.Ref

import org.dabr.rng4cats.Random

final class Key(val s: String) extends AnyVal
final class Hash(val l: Long) extends AnyVal
final class Value(val i: Int) extends AnyVal

/**
 * Represents a K->V storage, where we never always append a random hash to our key, to avoid
 * writing to the same location twice
 */
trait Store[F[_]] {
  // returns true if the (Key, Hash) pair is unique
  def put(k: Key, hash: Hash, v: Value): F[Boolean]
  def get(k: Key): F[Option[Value]]
  def hashes(k: Key): F[List[Hash]]
}

final case class RefStore[F[_]: Monad](ref: Ref[F, Map[(Key, Hash), Value]]) extends Store[F] {
  def put(k: Key, hash: Hash, v: Value): F[Boolean] = ref.modify { map =>
    map.contains((k, hash)) match {
      case true  => (map, false) // can't do the insertion
      case false => (map + ((k, hash) -> v), true)
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

  def parallelWrite[F[_]: Monad: Parallel](
      writes: List[(Key, Value)],
      store: Store[F],
      rngRef: Ref[F, Random]
  ): F[Unit] = {
    val writesWithHash: F[List[(Key, Value)]] = writes.parTraverse {
      case (k, v) =>
        rngRef.modify { rng =>
          ???
        }
    }
    ???
  }

  def main(args: Array[String]): Unit = {
    implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
    val keys: List[Key] =
      List("foo", "foo", "foo", "bar", "bar", "bar", "baz", "baz", "baz").map(new Key(_))
    val values: List[Value] = List(1, 2, 3, 4, 5, 6, 7, 8, 9).map(new Value(_))
    val io: IO[Unit] = for {
      ref <- Ref.of[IO, Map[(Key, Hash), Value]](Map.empty)
      store = RefStore[IO](ref)
    } yield ???
  }

}

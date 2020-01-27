package org.dabr.randomcats.examples

import cats.effect.concurrent.Ref
import cats.Functor
import cats.implicits._

/**
 * Our storage abstraction which requires each key to have a random hash associated with it
 * to avoid write collisions.
 */
final case class Key(val s: String) extends AnyVal
final case class Hash(val l: Long) extends AnyVal
final case class Value(val i: Int) extends AnyVal

/**
 * Represents a K->V storage, where we always append a random hash to our key, to avoid writing to
 * the same location twice
 */
trait HashedStore[F[_]] {
  // returns true if the (Key, Hash) pair is unique, and got added to the store
  def put(k: Key, hash: Hash, v: Value): F[Boolean]
  def get(k: Key): F[Option[Value]]
  def hashes(k: Key): F[List[Hash]]
}

final class RefHashedStore[F[_]: Functor](ref: Ref[F, Map[(Key, Hash), Value]])
    extends HashedStore[F] {
  def put(k: Key, hash: Hash, v: Value): F[Boolean] = ref.modify { map =>
    val key = (k, hash)
    map.contains(key) match {
      case true  => (map, false) // can't do the insertion
      case false => (map + (key -> v), true)
    }
  }
  def get(k: Key): F[Option[Value]] = ref.get.map { map =>
    map.collectFirst {
      case ((key, _), value) if key == k => value
    }
  }

  def hashes(k: Key): F[List[Hash]] = ref.get.map { map =>
    map.keys.collect {
      case (k0, h) if k0 == k => h
    }.toList
  }
}

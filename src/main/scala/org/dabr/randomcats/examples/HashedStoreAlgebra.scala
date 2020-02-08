package org.dabr.randomcats.examples

import cats.~>

object HashedStoreAlgebra {
  sealed trait HashedStoreDSL[A]
  final case class Put(k: Key, hash: Hash, v: Value) extends HashedStoreDSL[Boolean]
  final case class Get(k: Key) extends HashedStoreDSL[Option[Value]]
  final case class Hashes(k: Key) extends HashedStoreDSL[List[Hash]]

  def compiler[F[_]](interp: HashedStore[F]): HashedStoreDSL ~> F = new (HashedStoreDSL ~> F) {
    override def apply[A](fa: HashedStoreDSL[A]): F[A] = {
      fa match {
        case Put(k, h, v) => interp.put(k, h, v)
        case Get(k)       => interp.get(k)
        case Hashes(k)    => interp.hashes(k)
      }
    }
  }
}

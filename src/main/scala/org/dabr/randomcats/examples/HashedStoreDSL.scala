package org.dabr.randomcats.examples

import cats.{~>, Monad, Applicative, Functor}
import cats.free._
import cats.free.Free._

object HashedStoreDSL {
  type FreeHashedStore[A] = Free[HashedStoreDSL, A]
  sealed trait HashedStoreDSL[A]
  final case class Put(k: Key, hash: Hash, v: Value) extends HashedStoreDSL[Boolean]
  final case class Get(k: Key) extends HashedStoreDSL[Option[Value]]
  final case class Hashes(k: Key) extends HashedStoreDSL[List[Hash]]

  def put(k: Key, hash: Hash, v: Value): FreeHashedStore[Boolean] = liftF(Put(k, hash, v))
  def get(k: Key): FreeHashedStore[Option[Value]] = liftF(Get(k))
  def hashes(k: Key): FreeHashedStore[List[Hash]] = liftF(Hashes(k))
  // def get(k: Key) extends HashedStoreDSL[Option[Value]]
  // def hashes(k: Key) extends HashedStoreDSL[List[Hash]]
  //
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

package org.dabr.randomcats.examples

import cats.{~>, Monad, Applicative, Functor}
import cats.free.Free.liftF
import cats.free.{Free, FreeApplicative}

object HashedStoreDSL {
  type HashedStoreMonad[A] = Free[HashedStoreDSL, A]
  type HashedStoreApplicative[A] = FreeApplicative[HashedStoreDSL, A]
  sealed trait HashedStoreDSL[A]
  final case class Put(k: Key, hash: Hash, v: Value) extends HashedStoreDSL[Boolean]
  final case class Get(k: Key) extends HashedStoreDSL[Option[Value]]
  final case class Hashes(k: Key) extends HashedStoreDSL[List[Hash]]

  def put(k: Key, hash: Hash, v: Value): HashedStoreMonad[Boolean] = liftF(Put(k, hash, v))
  def get(k: Key): HashedStoreMonad[Option[Value]] = liftF(Get(k))
  def hashes(k: Key): HashedStoreMonad[List[Hash]] = liftF(Hashes(k))
  def putAp(k: Key, hash: Hash, v: Value): HashedStoreApplicative[Boolean] =
    FreeApplicative.lift(Put(k, hash, v))
  def getAp(k: Key): HashedStoreApplicative[Option[Value]] = FreeApplicative.lift(Get(k))
  def hashesAp(k: Key): HashedStoreApplicative[List[Hash]] = FreeApplicative.lift(Hashes(k))
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

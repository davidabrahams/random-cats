package org.dabr.randomcats.examples

import cats.{Monad, Applicative, Parallel, InjectK}
import cats.data.EitherK
import cats.free.Free
import cats.effect._
import cats.implicits._
import cats.effect.concurrent._
import shapeless._

import scala.concurrent._

import org.dabr.randomcats._

object FreeRandom {

  sealed trait RandomDSL[A]
  case object RandomLong extends RandomDSL[Long]

  import HashedStoreDSL._

  type DSL[A] = EitherK[RandomDSL, HashedStoreDSL, A]
  type Program[A] = Free[DSL, A]

  // type Program[A] = Free[HashedStoreApplicative, A] // maybe later

  /**
   * Returns true if all writes were successful
   */
  def parallelWriteFree(
      writes: List[(Key, Value)]
  ): Program[Boolean] =
    writes
      .traverse {
        case (k, v) =>
          Free.inject[RandomDSL, DSL](RandomLong).map { long =>
            (k, v, Hash(long))
          }
      }
      .flatMap { list =>
        list.traverse {
          case (k, v, h) =>
            Free.inject[HashedStoreDSL, DSL](Put(k, h, v))
        }
      }
      .map { list =>
        list.forall(identity)
      }

  /**
   * Returns true if all writes were successful
   * Make sure you pass an Applicative F here
   */
  def parallelWrite[F[_]: Applicative](
      writes: List[(Key, Value)],
      rngRef: RandomRef[F]
  ): F[HashedStoreApplicative[Boolean]] =
    writes
      .traverse {
        case (k, v) =>
          rngRef.use(Random.nextLong).map { l =>
            HashedStoreDSL.putAp(k, new Hash(l), v)
          }
      }
      .map { list =>
        list.sequence.map(l => l.forall(identity))
      }

}

object FreeExample {
  def main(args: Array[String]): Unit = {
    println("Hello World")
  }
}

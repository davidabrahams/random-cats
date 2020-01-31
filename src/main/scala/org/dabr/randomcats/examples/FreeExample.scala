package org.dabr.randomcats.examples

import cats.{Monad, Applicative, Parallel, InjectK, ~>, Inject}
import cats.data.EitherK
import cats.free.{Free, FreeApplicative => FreeAp}
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

  type FreeS[F[_], A] = Free[({ type L[B] = FreeAp[F, B] })#L, A]
  object FreeS {
    type Par[F[_], A] = FreeAp[F, A]
  }

  type DSL[A] = EitherK[RandomDSL, HashedStoreDSL, A]
  type Program[A] = Free[DSL, A]
  type ParProgram[A] = FreeS.Par[DSL, A]
  type ParallelProgram[A] = FreeS[DSL, A]

  /**
   * This could be even more generic, on some G[_[_]]: FunctorK, and any F[_]: Applicative
   */
  def liftAp[F[_], A](free: Free[F, A]): FreeS[F, A] = {
    type FreeApPartApplied[B] = FreeAp[F, B]
    free.mapK {
      new (F ~> ({ type G[B] = FreeAp[F, B] })#G) {
        def apply[A](fa: F[A]): FreeAp[F, A] = FreeAp.lift(fa)
      }
    }
  }

  def parallelWriteFree(
      writes: List[(Key, Value)]
  ): ParallelProgram[Boolean] = {
    // first, get RNG values for each write in sequence using Monad
    val writesWithRng: Free[DSL, List[(Key, Value, Hash)]] = writes.traverse {
      case (k, v) =>
        Free.inject[RandomDSL, DSL](RandomLong).map { long =>
          (k, v, Hash(long))
        }
    }
    val apWrites: FreeS[DSL, List[(Key, Value, Hash)]] = liftAp(writesWithRng)
    apWrites.flatMap { l: List[(Key, Value, Hash)] =>
      Free.liftF(
        l.traverse {
            case (k, v, h) =>
              val dsl: DSL[Boolean] = InjectK[HashedStoreDSL, DSL].inj(Put(k, h, v))
              FreeAp.lift[DSL, Boolean](dsl)
          }
          .map { list =>
            list.forall(identity)
          }
      )
    }
  }

  /**
   * Returns true if all writes were successful
   */
  def sequentialWriteFree(
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

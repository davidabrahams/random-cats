package org.dabr.randomcats.examples

import cats.{Monad, Applicative, Parallel}
import cats.effect._
import cats.implicits._
import cats.effect.concurrent._

import scala.concurrent._

import org.dabr.randomcats._

object FreeRandom {

  import HashedStoreDSL._

  /**
   * Returns true if all writes were successful
   */
  def parallelWrite[F[_]: Monad: Parallel](
      writes: List[(Key, Value)],
      rngRef: RandomRef[F]
  ): F[HashedStoreApplicative[Boolean]] =
    writes
      .parTraverse {
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

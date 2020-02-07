package org.dabr.randomcats

import java.util.UUID
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration.NANOSECONDS

import cats.{Applicative, Functor}
import cats.data.{State, StateT}
import cats.effect.{Clock, Sync}
import cats.effect.concurrent.Ref
import cats.implicits._

final case class Seed(val l: Long) extends AnyVal

trait Random {
  def nextLong: (Random, Long)
  def nextInt: (Random, Int)
  def nextDouble: (Random, Double)
  def nextFloat: (Random, Float)

  /**
   * Allocates a new, mutable, array of random bytes
   */
  def nextBytes(numBytes: Int): (Random, Array[Byte])

  /**
   * This is unsafe because it mutates the user supplied byte array. You probably want
   * [[nextBytes]]. Note that it still returns the next [[Random]] instance! Take care not to reuse
   * the one you passed to this function
   */
  def unsafeNextBytes(bytes: Array[Byte]): Random

  /**
   * Produces an infinite [[LazyList]] of As, and the updated Random instance
   */
  def stream[A](f: Random => (Random, A)): LazyList[(Random, A)]

  /**
   * Produces a list of A's, and the [[Random]] instance created after producing the final A
   * value. If n is 0, [[listOf]] returns an empty list, and the same [[Random]] instance right
   * back.
   */
  def listOf[A](n: Int, f: Random => (Random, A)): (Random, List[A])

  def nextUUID: (Random, UUID)
}

final protected[randomcats] case class RandomImpl(s: Seed) extends Random {
  import RandomImpl._

  def next(bits: Int): (RandomImpl, Int) = {
    val nextSeed: Long = (s.l * multiplier + addend) & mask
    (new RandomImpl(new Seed(nextSeed)), (nextSeed >>> (48 - bits)).toInt)
  }

  def nextLong: (Random, Long) = {
    val (rng1, int1) = this.nextInt
    val (rng2, int2) = rng1.nextInt
    val long = (int1.toLong << 32) + int2
    (rng2, long)
  }

  def nextInt: (Random, Int) = next(32)

  def nextDouble: (Random, Double) = {
    val (rng1, int1) = this.next(26)
    val (rng2, int2) = rng1.next(27)
    (rng2, ((int1.toLong << 27) + int2) * double_unit)
  }

  def nextFloat: (Random, Float) = {
    val (rng1, int1) = this.next(24)
    (rng1, int1 / (1 << 24).toFloat)
  }

  def nextBytes(numBytes: Int): (Random, Array[Byte]) = {
    val bytes = new Array[Byte](numBytes)
    val rng0 = unsafeNextBytes(bytes)
    (rng0, bytes)
  }

  def unsafeNextBytes(bytes: Array[Byte]): Random = {

    /**
     * The idea is that we generate (bytes.length/4) random ints, and then use each int to fill 4
     * slots in the array (Integers are 32 bits, Bytes are 8)
     */
    var i = 0 // index into byte array
    var rngVar: Random = this
    val len = bytes.length
    while (i < len) { // fill all slots in the byte array
      // generate a random int
      val nextIntPair: (Random, Int) = rngVar.nextInt
      rngVar = nextIntPair._1
      var rnd: Int = nextIntPair._2
      // we use this int to fill 4 slots in the byte array, unless we have less than 4 slots
      // remaining
      var n = math.min(len - i, 4)
      while (n > 0) {
        n -= 1
        bytes(i) = rnd.toByte
        i += 1
        rnd = rnd >> 8
      }
    }
    rngVar
  }

  def stream[A](f: Random => (Random, A)): LazyList[(Random, A)] =
    LazyList.iterate(f(this)) { case (rng, _) => f(rng) }

  def listOf[A](n: Int, f: Random => (Random, A)): (Random, List[A]) = {
    n match {
      case _ if n < 0 => throw new java.lang.IllegalArgumentException(s"$n is negative")
      case 0          => (this, Nil)
      case _ =>
        val (streamWithoutTail, tail) = this.stream(f).splitAt(n - 1)
        // we know stream returns an infinite stream, so tail has a head.
        val (lastRng, lastA) = tail.head
        val streamAs: LazyList[A] = streamWithoutTail.map(_._2) :+ lastA
        (lastRng, streamAs.toList)
    }
  }

  /**
   * This matches the implementation of [[java.util.UUID.randomUUID]]. Note that it is not secure!
   * This uses the default Java random 48-bit linear congruential formula to generate the UUID.
   */
  def nextUUID: (Random, UUID) = {
    val (rng, bytes) = nextBytes(16)
    var msb = 0L
    var lsb = 0L
    for (i <- 0 until 8) {
      msb = (msb << 8) | (bytes(i) & 0xff);
    }
    for (i <- 8 until 16) {
      lsb = (lsb << 8) | (bytes(i) & 0xff);
    }
    (rng, new UUID(msb, lsb))
  }
}

object RandomImpl {
  def initialScramble(l: Long): Seed = new Seed((l ^ multiplier) & mask)
  val double_unit: Double = 1.0 / (1L << 53)
  val mask: Long = (1L << 48) - 1;
  val multiplier: Long = 0x5DEECE66DL;
  val addend: Long = 0xBL;
}

/**
 * We provide every function available on the [[Random]] class as a function on this companion, which accepts a
 * [[Random]] instance as an argument. This makes it easy to interop [[Random]] with [[cats.data.State]]
 */
object Random {
  private val seedUniquifier: AtomicLong = new AtomicLong(8682522807148012L)
  def apply(seed: Long): Random = new RandomImpl(RandomImpl.initialScramble(seed))

  /**
   * Returns a [[Random]] instance, seeded by the current time
   */
  def fromClock[F[_]](clock: Clock[F])(implicit F: Functor[F]): F[Random] =
    F.map(clock.monotonic(NANOSECONDS))(Random(_))

  /**
   * Returns a [[Random]] instance, seeded by the current time. This is different from [[fromClock]]
   * in that if there are simulataneous calls to uniqueFromClock, the returned [[Random]]
   * instances will be unique. Each call updates local mutable state to guarantee uniqueness. This
   * function is pure, as the mutable state updates are suspended in the F[_] context.
   * This matches the default [[java.util.Random]] constructor.
   */
  def uniqueFromClock[F[_]](clock: Clock[F])(implicit F: Sync[F]): F[Random] =
    for {
      unique <- F.delay {
        seedUniquifier.updateAndGet { l =>
          l * 181783497276652981L
        }
      }
      nanos <- clock.monotonic(NANOSECONDS)
    } yield Random(unique ^ nanos)

  def nextLong: Random => (Random, Long) = rng => rng.nextLong
  def nextInt: Random => (Random, Int) = rng => rng.nextInt
  def nextDouble: Random => (Random, Double) = rng => rng.nextDouble
  def nextFloat: Random => (Random, Float) = rng => rng.nextFloat
  def nextBytes(numBytes: Int): Random => (Random, Array[Byte]) = rng => rng.nextBytes(numBytes)
  def unsafeNextBytes(bytes: Array[Byte]): Random => Random = rng => rng.unsafeNextBytes(bytes)
  def stream[A](f: Random => (Random, A)): Random => LazyList[(Random, A)] = rng => rng.stream(f)
  def listOf[A](n: Int, f: Random => (Random, A)): Random => (Random, List[A]) =
    rng => rng.listOf(n, f)
  def nextUUID: Random => (Random, UUID) = rng => rng.nextUUID

  def stateT[F[_], A](f: Random => (Random, A))(implicit F: Applicative[F]): StateT[F, Random, A] =
    StateT(f.andThen(F.pure))

  def state[A](f: Random => (Random, A)): State[Random, A] =
    State(f)

  def ref[F[_]](rng: Random)(implicit F: Sync[F]): F[RandomRef[F]] =
    F.map(Ref.of[F, Random](rng))(new RandomRefImpl(_))
}

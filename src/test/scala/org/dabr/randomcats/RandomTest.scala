package org.dabr.randomcats

import org.scalatest.FunSuite
import org.scalacheck.{Arbitrary, Gen}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import java.util.{Random => JRandom}
import scala.concurrent.duration.TimeUnit

import cats.Id
import cats.effect.{IO, Clock}

class RandomTest extends FunSuite with ScalaCheckDrivenPropertyChecks {

  /**
   * Tests that successive calls to produce a random A using Java's RNG and our own produce
   * identical values
   */
  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  private def testSuccessive[A](
      calls: Int,
      jRng: JRandom,
      sRng: Random,
      jFunc: JRandom => A,
      sFunc: Random => (Random, A),
      // by default we use universal equality, but we may want to override (eg, to compare Arrays)
      compare: (A, A) => Boolean = (a0: A, a1: A) => a0 == a1
  ): Unit = {
    var scalaRng = sRng
    for (_ <- 0 until calls) {
      val (rng0, scalaA) = sFunc(scalaRng)
      val javaA = jFunc(jRng)
      scalaRng = rng0
      assert(compare(scalaA, javaA))
    }
  }

  test("matches Java nextBytes") {
    forAll(Arbitrary.arbitrary[Long], Gen.choose(0, 20), Gen.choose(0, 100)) {
      (l: Long, calls: Int, numBytes: Int) =>
        val rng: Random = Random(l)
        val javaRng: JRandom = new JRandom(l)
        testSuccessive[Array[Byte]](
          calls,
          javaRng,
          rng, { j =>
            val bytes = new Array[Byte](numBytes)
            j.nextBytes(bytes)
            bytes
          },
          Random.nextBytes(numBytes),
          (a0: Array[Byte], a1: Array[Byte]) => (a0.sameElements(a1))
        )
    }
  }

  test("matches Java nextInt on successive calls") {
    forAll(Arbitrary.arbitrary[Long], Gen.choose(0, 20)) { (l: Long, calls: Int) =>
      val rng: Random = Random(l)
      val javaRng: JRandom = new JRandom(l)
      testSuccessive[Int](calls, javaRng, rng, j => j.nextInt, Random.nextInt)
    }
  }

  test("matches Java nextLong on successive calls") {
    forAll(Arbitrary.arbitrary[Long], Gen.choose(0, 20)) { (l: Long, calls: Int) =>
      val rng: Random = Random(l)
      val javaRng: JRandom = new JRandom(l)
      testSuccessive[Long](calls, javaRng, rng, j => j.nextLong, Random.nextLong)
    }
  }

  test("matches Java nextDouble on successive calls") {
    forAll(Arbitrary.arbitrary[Long], Gen.choose(0, 20)) { (l: Long, calls: Int) =>
      val rng: Random = Random(l)
      val javaRng: JRandom = new JRandom(l)
      testSuccessive[Double](calls, javaRng, rng, j => j.nextDouble, Random.nextDouble)
    }
  }

  test("matches Java nextFloat on successive calls") {
    forAll(Arbitrary.arbitrary[Long], Gen.choose(0, 20)) { (l: Long, calls: Int) =>
      val rng: Random = Random(l)
      val javaRng: JRandom = new JRandom(l)
      testSuccessive[Float](calls, javaRng, rng, j => j.nextFloat, Random.nextFloat)
    }
  }

  test("listOf matches Java longs") {
    forAll(Arbitrary.arbitrary[Long], Gen.choose(0, 20)) { (l: Long, calls: Int) =>
      val rng: Random = Random(l)
      val javaRng: JRandom = new JRandom(l)
      val javaLongs: List[Long] = javaRng.longs(calls.toLong).toArray.toList
      val scalaLongs: List[Long] = Random.listOf(calls, Random.nextLong)(rng)._2
      assert(javaLongs == scalaLongs)
    }
  }

  test("concatting two random lists produces identical list") {
    forAll(Arbitrary.arbitrary[Long], Gen.choose(0, 10), Gen.choose(0, 10)) {
      (l: Long, length1: Int, length2) =>
        val rng: Random = Random(l)
        val f = Random.nextLong(_)
        val (rng0, subList1) = Random.listOf(length1, f)(rng)
        val (_, subList2) = Random.listOf(length2, f)(rng0)
        val fullList = Random.listOf(length1 + length2, f)(rng)._2
        assert(subList1 ++ subList2 == fullList)
    }
  }

  test("fromClock produces identical Random instances at identical times") {
    forAll(Arbitrary.arbitrary[Long]) {
      (time: Long) =>
        val clock: Clock[Id] = new Clock[Id] {
          def monotonic(unit: TimeUnit): Long = time
          def realTime(unit: TimeUnit): Long = time
        }
        assert(Random.fromClock[Id](clock) == Random.fromClock[Id](clock))
    }
  }

  test("fromClockUnique produces different Random instances at identical times") {
    forAll(Arbitrary.arbitrary[Long]) {
      (time: Long) =>
        val clock: Clock[IO] = new Clock[IO] {
          def monotonic(unit: TimeUnit)= IO.pure(time)
          def realTime(unit: TimeUnit)= IO.pure(time)
        }
        (for {
          rng1 <- Random.uniqueFromClock[IO](clock)
          rng2 <- Random.uniqueFromClock[IO](clock)
          _ <- IO(assert(rng1 != rng2))
        } yield ()).unsafeRunSync
    }
  }
}

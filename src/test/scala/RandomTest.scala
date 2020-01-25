import org.scalatest.FunSuite
import org.scalacheck.{Arbitrary, Gen}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import java.util.{Random => JRandom}

class RandomTest extends FunSuite with ScalaCheckDrivenPropertyChecks {

  private def testSuccessive[A](
      calls: Int,
      jRng: JRandom,
      sRng: Random,
      jFunc: JRandom => A,
      sFunc: Random => (Random, A)
  ): Unit = {
    var scalaRng = sRng
    for (_ <- 0 until calls) {
      val (rng0, scalaA) = sFunc(scalaRng)
      val javaA = jFunc(jRng)
      scalaRng = rng0
      assert(scalaA == javaA)
    }
  }

  test("matches Java nextInt on successive calls") {
    forAll(Arbitrary.arbitrary[Long], Gen.choose(0, 20)) { (l: Long, calls: Int) =>
      val rng: Random = Random(Seed(l))
      val javaRng: JRandom = new JRandom(l)
      testSuccessive(calls, javaRng, rng, j => j.nextInt, Random.nextInt)
    }
  }

  test("matches Java nextLong on successive calls") {
    forAll(Arbitrary.arbitrary[Long], Gen.choose(0, 20)) { (l: Long, calls: Int) =>
      val rng: Random = Random(Seed(l))
      val javaRng: JRandom = new JRandom(l)
      testSuccessive(calls, javaRng, rng, j => j.nextLong, Random.nextLong)
    }
  }

  test("listOf matches Java longs") {
    forAll(Arbitrary.arbitrary[Long], Gen.choose(0, 20)) { (l: Long, calls: Int) =>
      val rng: Random = Random(Seed(l))
      val javaRng: JRandom = new JRandom(l)
      val javaLongs: List[Long] = javaRng.longs(calls.toLong).toArray.toList
      val scalaLongs: List[Long] = Random.listOf(calls, Random.nextLong)(rng)._2
      assert(javaLongs == scalaLongs)
    }
  }

  test("concatting two random lists produces identical list") {
    forAll(Arbitrary.arbitrary[Long], Gen.choose(0, 10), Gen.choose(0, 10)) {
      (l: Long, length1: Int, length2) =>
        val rng: Random = Random(Seed(l))
        val f = Random.nextLong(_)
        val (rng0, subList1) = Random.listOf(length1, f)(rng)
        val (_, subList2) = Random.listOf(length2, f)(rng0)
        val fullList = Random.listOf(length1 + length2, f)(rng)._2
        assert(subList1 ++ subList2 == fullList)
    }
  }
}

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
}

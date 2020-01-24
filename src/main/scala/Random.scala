import java.util.{Random => JRandom}
import scala.annotation.tailrec
import scala.collection.immutable.LazyList
import cats.data.State


sealed trait Random {
  protected def next(bits: Int): (Random, Int)
}

final case class Seed(l: Long) extends AnyVal

final case class RandomImpl(s: Seed) extends Random {
  import RandomImpl._
  def next(bits: Int): (Random, Int) = {
    val nextSeed: Long = (s.l * multiplier + addend) & mask
    (RandomImpl(Seed(nextSeed)), (nextSeed >>> (48 - bits)).toInt)
  }
}

object RandomImpl {
  val mask: Long = (1L << 48) - 1;
  val multiplier: Long = 0x5DEECE66DL;
  val addend: Long = 0xBL;
}

object Random {
  import RandomImpl._
  def initialScramble(seed: Seed): Seed = Seed((seed.l ^ multiplier) & mask)
  def apply(seed: Seed): Random = RandomImpl(initialScramble(seed))

  def nextLong(rng: Random): (Random, Long) = {
    val (rng1, int1) = nextInt(rng)
    val (rng2, int2) = nextInt(rng1)
    val long = (int1.toLong << 32) + int2
    (rng2, long)
  }
  def nextInt(rng: Random): (Random, Int) = rng.next(32)

  def randomLongs(n: Int)(rng: Random): (Random, List[Long]) = Random.tailRecList(n, Nil, rng, nextLong)
  def randomInts(n: Int)(rng: Random): (Random, List[Int]) = Random.tailRecList(n, Nil, rng, nextInt)

  def stream[A](take: Random => (Random, A))(rng: Random): LazyList[(Random, A)] = {
    val first: (Random, A) = take(rng)
    // TODO: this breaks with stackoverflow...
    def lazylist: LazyList[(Random, A)] = first #:: lazylist.tail.map {
      case (rng, a) => take(rng)
    }
    lazylist
  }

  @tailrec private def tailRecList[A](n: Int, acc: List[A], rng: Random, func: Random => (Random, A)): (Random, List[A]) =
    n match {
      case _ if n < 0 => sys.error("Bad!")
      case 0 => (rng, acc)
      case _ =>
        val (rng0, a) = func(rng)
        tailRecList(n-1, a :: acc, rng0, func)
    }
}

object Main {
  def main(args: Array[String]): Unit = {
    val seed = 42
    val javaRandom = new JRandom(seed)
    // test nextLong twice against Java
    val rng0 = Random(Seed(seed))
    val jlong1 = javaRandom.nextLong()
    val (rng1, slong1) = Random.nextLong(rng0)
    assert(jlong1 == slong1)
    val jlong2 = javaRandom.nextLong()
    val (rng2, slong2) = Random.nextLong(rng1)
    assert(jlong2 == slong2)

    // test nextInt
    val jint = javaRandom.nextInt()
    val (rng3, sint) = Random.nextInt(rng2)
    assert(jint == sint)


    println(Random.stream(Random.nextInt)(rng3).take(5).toList)

  }
}

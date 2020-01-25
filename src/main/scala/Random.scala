sealed trait Random {
  protected def next(bits: Int): (Random, Int)
}

final case class Seed(l: Long) extends AnyVal

final private class RandomImpl(s: Seed) extends Random {
  import RandomImpl._
  override def toString: String = s"RandomImpl(Seed=${s.l})"
  def next(bits: Int): (Random, Int) = {
    val nextSeed: Long = (s.l * multiplier + addend) & mask
    (new RandomImpl(Seed(nextSeed)), (nextSeed >>> (48 - bits)).toInt)
  }
}

object RandomImpl {
  def initialScramble(s: Seed): Seed = Seed((s.l ^ multiplier) & mask)
  val mask: Long = (1L << 48) - 1;
  val multiplier: Long = 0x5DEECE66DL;
  val addend: Long = 0xBL;
}

object Random {
  def apply(seed: Seed): Random = new RandomImpl(RandomImpl.initialScramble(seed))

  def nextLong(rng: Random): (Random, Long) = {
    val (rng1, int1) = nextInt(rng)
    val (rng2, int2) = nextInt(rng1)
    val long = (int1.toLong << 32) + int2
    (rng2, long)
  }
  def nextInt(rng: Random): (Random, Int) = rng.next(32)

  def stream[A](f: Random => (Random, A))(rng: Random): LazyList[(Random, A)] =
    LazyList.iterate(f(rng)) { case (rng, _) => f(rng) }

  def listOf[A](n: Int, f: Random => (Random, A))(rng: Random): (Random, List[A]) = {
    n match {
      case _ if n < 0 => throw new java.lang.IllegalArgumentException(s"$n is negative")
      case 0          => (rng, Nil)
      case _ =>
        val (streamWithoutTail, tail) = stream(f)(rng).splitAt(n - 1)
        // we know stream returns an infinite stream, so tail has a head.
        val (lastRng, lastA) = tail.head
        val streamAs: LazyList[A] = streamWithoutTail.map(_._2) :+ lastA
        (lastRng, streamAs.toList)
    }
  }
}

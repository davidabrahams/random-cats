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
  private val double_unit: Double = 1.0 / (1L << 53)

  def apply(seed: Seed): Random = new RandomImpl(RandomImpl.initialScramble(seed))

  def nextLong(rng: Random): (Random, Long) = {
    val (rng1, int1) = nextInt(rng)
    val (rng2, int2) = nextInt(rng1)
    val long = (int1.toLong << 32) + int2
    (rng2, long)
  }

  def nextInt(rng: Random): (Random, Int) = rng.next(32)

  def nextDouble(rng: Random): (Random, Double) = {
    val (rng1, int1) = rng.next(26)
    val (rng2, int2) = rng1.next(27)
    (rng2, ((int1.toLong << 27) + int2) * double_unit)
  }

  def nextFloat(rng: Random): (Random, Float) = {
    val (rng1, int1) = rng.next(24)
    (rng1, int1 / (1 << 24).toFloat)
  }

  /**
   * Allocates a new, mutable, array of random bytes
   */
  def nextBytes(numBytes: Int)(rng: Random): (Random, Array[Byte]) = {
    val bytes = new Array[Byte](numBytes)
    val rng0 = unsafeNextBytes(bytes)(rng)
    (rng0, bytes)
  }

  /**
   * This is unsafe because it mutates the user supplied byte array. You probably want
   * [[nextBytes]]. Note that it still returns the next [[Random]] instance! Take care not to reuse
   * the one you passed to this function
   */
  def unsafeNextBytes(bytes: Array[Byte])(rng: Random): Random = {

    /**
     * The idea is that we generate (bytes.length/4) random ints, and then use each int to fill 4
     * slots in the array (Integers are 32 bits, Bytes are 8)
     */
    var i = 0 // index into byte array
    var rngVar = rng
    val len = bytes.length
    while (i < len) { // fill all slots in the byte array
      // generate a random int
      val nextIntPair: (Random, Int) = Random.nextInt(rngVar)
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

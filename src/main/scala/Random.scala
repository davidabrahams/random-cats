sealed trait Random {
  final def nextLong: (Random, Long) = {
    val (rng1, int1) = this.next(32)
    val (rng2, int2) = rng1.next(32)
    val long = (int1 << 32).toLong + int2
    (rng2, long)
  }
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
  def main(args: Array[String]): Unit = {
  }
}

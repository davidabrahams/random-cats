sealed trait Random {
  def nextLong: (Random, Long)
  protected def next(bits: Int): (Random, Int)
}

final case class Seed(l: Long) extends AnyVal

final case class RandomImpl(s: Seed) extends Random {
  import RandomImpl._
  def nextLong: (Random, Long) = ???
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

}

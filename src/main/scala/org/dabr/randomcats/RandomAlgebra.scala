package org.dabr.randomcats

import java.util.UUID
import cats.arrow.FunctionK

sealed trait RandomAlgebra[A]
final case object RandomLong extends RandomAlgebra[Long]
final case object RandomInt extends RandomAlgebra[Int]
final case object RandomDouble extends RandomAlgebra[Double]
final case object RandomFloat extends RandomAlgebra[Float]
final case class RandomBytes(n: Int) extends RandomAlgebra[Array[Byte]]
final case object RandomUUID extends RandomAlgebra[UUID]

object RandomAlgebra {
  def long: RandomAlgebra[Long] = RandomLong
  def int: RandomAlgebra[Int] = RandomInt
  def double: RandomAlgebra[Double] = RandomDouble
  def float: RandomAlgebra[Float] = RandomFloat
  def bytes(n: Int): RandomAlgebra[Array[Byte]] = RandomBytes(n)
  def uuid: RandomAlgebra[UUID] = RandomUUID
  def refCompiler[F[_]](ref: RandomRef[F]): FunctionK[RandomAlgebra, F] =
    new FunctionK[RandomAlgebra, F] {
      def apply[A](fa: RandomAlgebra[A]): F[A] = fa match {
        case RandomLong     => ref.next(Random.nextLong)
        case RandomInt      => ref.next(Random.nextInt)
        case RandomDouble   => ref.next(Random.nextDouble)
        case RandomFloat    => ref.next(Random.nextFloat)
        case RandomBytes(n) => ref.next(Random.nextBytes(n))
        case RandomUUID     => ref.next(Random.nextUUID)
      }
    }
}

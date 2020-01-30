package org.dabr.randomcats.examples

import cats._
import cats.effect._
import cats.implicits._
import cats.effect.concurrent._

import scala.concurrent._

import org.dabr.randomcats._

object FreeRandom {
  sealed trait RandomDSL[A]

}

object FreeExample {
  def main(args: Array[String]): Unit = {
    println("Hello World")
  }
}

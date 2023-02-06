package fpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class FloatingPointSpec extends AnyFlatSpec {
  behavior of "FloatingPoint"

  val r = scala.util.Random

  it should "Make and Open Scala Floats" in {
    for (i <- 0 until 200) {
      val n = -1f + (2) * r.nextFloat()
      assert(FloatingPoint.open(FloatingPoint.make(n)) == n)
    }
  }
}

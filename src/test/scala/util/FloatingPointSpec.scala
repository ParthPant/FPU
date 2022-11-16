package fpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class FloatingPointSpec extends AnyFlatSpec {
    behavior of "FloatingPoint"

    val r = scala.util.Random

    it should "Make and Open Scala Floats" in {
        for (i <- 0 until 20) {
            val n = -10000f + (20000f) * r.nextFloat() 
            assert(FloatingPoint.open(FloatingPoint.make(n)) == n)
        }
    }
}

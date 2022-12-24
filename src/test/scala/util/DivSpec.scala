package fpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ArrayDividerSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ArrayDivider"

  val r = scala.util.Random
  
  for (w <- List(8, 16, 24, 32)) {
    val hi = BigInt(2).pow(w)
    it should s"Divider $w-bit numbers" in {
      for (i <- 1 to 10) {
        test (new ArrayDivider(w)) { c =>
          val a = BigInt(w, r)
          val b = BigInt(w, r)
          val (zhi, d) = (a min b, a max b)
          val zlo = BigInt(w, r)
          val z = (zhi<<w)|zlo
          val Q = z/d

          c.io.z.poke(z.U)
          c.io.d.poke(d.U)

          val out = c.io.Q.peekInt()
          // println(s"${Q == ${out.toString(2)}")
          c.io.Q.expect(Q.U)
        }
      }
    }
  }
}

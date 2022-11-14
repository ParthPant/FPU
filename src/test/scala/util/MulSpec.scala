package fpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ArrayMultiplierSpec extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "ArrayMultiplier"

    val r = scala.util.Random

    for (w <- List(8, 16, 32, 64)) {
        val hi = BigInt(2).pow(w)
        it should s"Multiply $w bit numbers" in {
            test (new ArrayMultiplier(w)) { c =>
                val a = BigInt(w, r)
                val b = BigInt(w, r)
                val prod = a * b

                c.io.a.poke(a.U)
                c.io.b.poke(b.U)

                c.io.P.expect(prod)
            }
        }
    }
}

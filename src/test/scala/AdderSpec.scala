package fpu

import chisel3._
import chiseltest._
import scala.math.pow
import org.scalatest.flatspec.AnyFlatSpec

class FastAdderSpec extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "FastAdder"

    val r = scala.util.Random
    for (w <- List(8, 16, 24, 28, 32, 64)) {
        val hi = BigInt(2).pow(w)
        for (v <- List(1, 2, 4)) {
            it should s"Add $w bit numbers with valency $v" in {
                test (new FastAdder(w, v)) { c =>
                    for (i <- 1 to 500) {
                        val a = BigInt(w, r)
                        val b = BigInt(w, r)
                        val cin = r.nextInt(2)
                        val cout = if (a + b + cin >=  hi) 1 else 0
                        val sum = (a+b+cin) & (hi-1) 

                        c.io.a.poke(a.U)
                        c.io.b.poke(b.U)
                        c.io.cin.poke(cin.U)

                        c.io.Sum.expect(sum.U)
                        c.io.Cout.expect(cout.U)
                    }
                }
            }
        }
    }
}
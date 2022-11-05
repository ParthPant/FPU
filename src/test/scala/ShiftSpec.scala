package fpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ShiftRightSpec extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "ShiftRight"

    val r = scala.util.Random

    for (w <- List(2, 4, 8, 16, 24, 32, 48, 64)) {
        it should s"Shift $w bit number to Right" in {
            val data = BigInt(w, r)
            val shift = r.nextInt(w)
            test (new ShiftRight(w)) { c =>
                c.io.a.poke(data.U)
                c.io.shift.poke(shift.U)
                c.io.out.expect((data>>shift).U)
            }
        }
    }
}

class ShiftLeftSpec extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "ShiftLeft"

    val r = scala.util.Random

    for (w <- List(2, 4, 8, 16, 24, 32, 48, 64)) {
        it should s"Shift $w bit number to Left" in {
            val data = BigInt(w, r)
            val shift = r.nextInt(w)
            test (new ShiftLeft(w)) { c =>
                c.io.a.poke(data.U)
                c.io.shift.poke(shift.U)
                c.io.out.expect((data<<shift & (BigInt(2).pow(w) - 1)).U)
            }
        }
    }
}
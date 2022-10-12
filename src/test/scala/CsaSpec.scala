package fpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


class CsaSpec extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "Adder"

    it should "Add 3 nums" in {
        test (new CarrySaveAdder(3)) { c =>
            c.io.a.poke("b011".U)
            c.io.b.poke("b101".U)
            c.io.c.poke("b010".U)

            c.io.S.expect("b100".U)
            c.io.C.expect("b011".U)
        }

        test (new CarrySaveAdder(5)) { c =>
            c.io.a.poke("b01110".U)
            c.io.b.poke("b10100".U)
            c.io.c.poke("b01001".U)

            c.io.S.expect("b10011".U)
            c.io.C.expect("b01100".U)
        }
    }
}
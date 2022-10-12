package fpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


class KsaSpec extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "Adder"

    it should "Add without cout" in {
        test (new KoggeStoneAdder(2)) { c => 
            c.io.a.poke(1.U)
            c.io.b.poke(2.U)
            c.io.cin.poke(0.U)

            c.io.sum.expect(3.U)
            c.io.cout.expect(0.U)
        }
    }

    it should "Add with cout" in {
        test (new KoggeStoneAdder(2)) { c => 
            c.io.a.poke(1.U)
            c.io.b.poke(3.U)
            c.io.cin.poke(0.U)

            c.io.sum.expect(0.U)
            c.io.cout.expect(1.U)
        }
    }

    it should "Add with cin and cout" in {
        test (new KoggeStoneAdder(2)) { c => 
            c.io.a.poke(1.U)
            c.io.b.poke(2.U)
            c.io.cin.poke(1.U)

            c.io.sum.expect(0.U)
            c.io.cout.expect(1.U)
        }
    }
}
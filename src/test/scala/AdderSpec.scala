package fpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


class AdderSpec extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "Adder"

    it should "Add" in {
        test (new Adder(2)) { c => 
            c.io.in1.poke(1.U)
            c.io.in2.poke(3.U)
            c.io.cin.poke(1.U)
            c.io.sum.expect(1.U)
            c.io.cout.expect(1.U)
            println("Output Value: " + c.io.sum.peek().litValue)
        }
    }
}
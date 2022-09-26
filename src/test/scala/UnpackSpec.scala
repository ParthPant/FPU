package fpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class Unpack32Spec extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "Unpack32"

    it should "Unpack 32 bits" in {
        test (new Unpack32) { c => 
            val input = "b11010110011110000111100001010101".U

            c.io.in.poke(input)
            c.io.s.expect(1.U)
            c.io.e.expect("b10101100".U)
            c.io.f.expect("b11110000111100001010101".U)
        }
    }
}

class Unpack64Spec extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "Unpack64"

    it should "Unpack 64 bits" in {
        test (new Unpack64) { c => 
            val input = "b1101111010101101101111101110111101101001011010010000000100000001".U

            c.io.in.poke(input)
            c.io.s.expect(1.U)
            c.io.e.expect("b10111101010".U)
            c.io.f.expect("b1101101111101110111101101001011010010000000100000001".U)
        }
    }
}
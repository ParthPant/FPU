package fpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ClzSpec extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "Clz"
    
    it should "Nibble Local Count" in {
        test (new NLC) { c => 
            c.io.in.poke("b0001".U)
            c.io.a.expect(0.U)
            c.io.Z.expect(3.U)
        }

        test (new NLC) { c => 
            c.io.in.poke("b0101".U)
            c.io.a.expect(0.U)
            c.io.Z.expect(1.U)
        }

        test (new NLC) { c => 
            c.io.in.poke("b0010".U)
            c.io.a.expect(0.U)
            c.io.Z.expect(2.U)
        }

        test (new NLC) { c => 
            c.io.in.poke("b1101".U)
            c.io.a.expect(0.U)
            c.io.Z.expect(0.U)
        }

        test (new NLC) { c => 
            c.io.in.poke("b0000".U)
            c.io.a.expect(1.U)
        }
    }

    it should "Count Leading Zeros 32-bit" in {
        test (new CLZ32) { c =>
            c.io.in.poke("h003FFFFF".U)
            c.io.Z.expect(10.U)
            c.io.a.expect(0.U)
        }

        test (new CLZ32) { c =>
            c.io.in.poke("h0002FFFF".U)
            c.io.Z.expect(14.U)
            c.io.a.expect(0.U)
        }

        test (new CLZ32) { c =>
            c.io.in.poke("h1002FFFF".U)
            c.io.Z.expect(3.U)
            c.io.a.expect(0.U)
        }

        test (new CLZ32) { c =>
            c.io.in.poke("h8002FFFF".U)
            c.io.Z.expect(0.U)
            c.io.a.expect(0.U)
        }

        test (new CLZ32) { c =>
            c.io.in.poke("h00000000".U)
            c.io.a.expect(1.U)
        }
    }

    it should "Count Leading Zeros 64-bit" in {
        test (new CLZ64) { c =>
            c.io.in.poke("h003FFFFF003FFFFF".U)
            c.io.Z.expect(10.U)
            c.io.a.expect(0.U)
        }

        test (new CLZ64) { c =>
            c.io.in.poke("h000000000002FFFF".U)
            c.io.Z.expect(46.U)
            c.io.a.expect(0.U)
        }

        test (new CLZ64) { c =>
            c.io.in.poke("h1000000000000000".U)
            c.io.Z.expect(3.U)
            c.io.a.expect(0.U)
        }

        test (new CLZ64) { c =>
            c.io.in.poke("h80080000C50A0000".U)
            c.io.Z.expect(0.U)
            c.io.a.expect(0.U)
        }

        test (new CLZ64) { c =>
            c.io.in.poke("h0000000000000000".U)
            c.io.a.expect(1.U)
        }
    }
}
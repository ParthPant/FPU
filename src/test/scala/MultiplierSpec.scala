package fpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation


class MultiplierSpec extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "Multiplier"

    it should "Multiply" in {
        test (new Multiplier(32)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
            c.io.a.poke(32.U)
            c.io.x.poke(2.U)
            c.clock.step(65)
            c.io.p.expect(64.U)
        }

        test (new Multiplier(32)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
            c.io.a.poke(64.U)
            c.io.x.poke(2.U)
            c.clock.step(65)
            c.io.p.expect(128.U)
        }
    }
}
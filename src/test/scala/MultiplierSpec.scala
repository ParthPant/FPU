package fpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import chiseltest.simulator.WriteVcdAnnotation


class FourToTwoReducerSpec extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "FourToTwoReducer"

    it should "Reduce 4 to 2" in {
        test (new FourToTwoReducer(32)) { c => 
            val r = scala.util.Random
            val ns = for (i <- 0 until 4) yield (r.nextInt(1000)) 

            c.io.a.poke(ns(0).U)
            c.io.b.poke(ns(1).U)
            c.io.c.poke(ns(2).U)
            c.io.d.poke(ns(3).U)
            
            val S = c.io.S.peek().litValue
            val C = c.io.C.peek().litValue
        
            (S+C) shouldBe (ns.sum)
        }
    }
}
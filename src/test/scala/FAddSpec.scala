package fpu

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation

class FAddSpec extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "FAdd"

    val lnOf2 = scala.math.log(2) // natural log of 2
    def log2(x: Double): Double = scala.math.log(x) / lnOf2
    
    it should "Add two numbers" in {

        test (new FAdd).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
            val a = chiselTypeOf(c.io.x).Lit(_.sign -> 0.U, _.mant -> 34.U, _.exp -> 3.U)
            val b = chiselTypeOf(c.io.y).Lit(_.sign -> 0.U, _.mant -> 65.U, _.exp -> 3.U)
            val s = chiselTypeOf(c.io.out).Lit(_.sign -> 0.U, _.mant -> 99.U, _.exp -> 3.U)

            c.io.x.poke(a)
            c.io.y.poke(b)

            c.clock.step(3 + log2(24/4).toInt + 10)

            c.io.out.expect(s)
        }
    }
}

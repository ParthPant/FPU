package fpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation
import scala.collection.mutable.ListBuffer
import org.scalatest.matchers.should.Matchers._
import com.typesafe.scalalogging.LazyLogging

class FRootSpec extends AnyFlatSpec with ChiselScalatestTester with LazyLogging {
  behavior of "FRoot"

  val r = scala.util.Random
  val steps = 5

  it should "Calculate Square Root of a floating Point number" in {
    for (i <- 0 until 20) {
      test(new FRoot).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        val a = 1+r.nextFloat()
        val R: Float = scala.math.sqrt(a).toFloat

        c.io.a.poke(FloatingPoint.make(a))
        c.clock.step(steps)

        val res = FloatingPoint.open(c.io.out.peek())
        val del = R - res
        println(s"$a => $R ?= $res /=> ${del.abs} (${res/R})")
        // if (!R.isNaN) assert(del.abs < 1)
      }
    }
  }
}

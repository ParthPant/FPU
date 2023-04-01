package fpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation
import scala.collection.mutable.ListBuffer
import org.scalatest.matchers.should.Matchers._
import com.typesafe.scalalogging.LazyLogging

class FRootSpec
    extends AnyFlatSpec
    with ChiselScalatestTester
    with LazyLogging {
  behavior of "FRoot"

  val r = scala.util.Random
  val steps = 13

  it should "Calculate Square Root of a floating Point number" in {
    for (i <- 0 until 20) {
      test(new FRoot).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        val a = r.nextFloat()
        val R: Float = scala.math.sqrt(a).toFloat

        c.io.a.poke(FloatingPoint.make(a))
        c.clock.step(steps)

        val res = FloatingPoint.open(c.io.out.peek())
        val del = R - res
        if (!R.isNaN) {
          // println(s"$a => $R ?= $res /=> ${del.abs} (${res/R})")
          assert(del.abs < 0.1)
        }
      }
    }
    for (i <- 0 until 20) {
      test(new FRoot).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        val a = 1 + 20 * r.nextFloat()
        val R: Float = scala.math.sqrt(a).toFloat

        c.io.a.poke(FloatingPoint.make(a))
        c.clock.step(steps)

        val res = FloatingPoint.open(c.io.out.peek())
        val del = R - res
        if (!R.isNaN) {
          // println(s"$a => $R ?= $res /=> ${del.abs} (${res/R})")
          assert(del.abs < 0.1)
        }
      }
    }
  }

  val n = 10
  it should s"Calculate Square Root of $n pipelined floating Point numbers" in {
    test(new FRoot).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val ips = for (i <- 1 to n) yield {
        val a = 20 * r.nextFloat()
        val R: Float = scala.math.sqrt(a).toFloat
        (a, R)
      }

      val outBuffer = new ListBuffer[Float]()
      for (ip <- ips) {
        c.io.a.poke(FloatingPoint.make(ip._1))
        val o = FloatingPoint.open(c.io.out.peek())
        outBuffer += o
        c.clock.step(1)
      }

      for (i <- 1 to steps) {
        val o = FloatingPoint.open(c.io.out.peek())
        outBuffer += o
        c.clock.step(1)
      }

      val outList = outBuffer.toList.drop(outBuffer.size - n)
      val dels = ips.zip(outList).map {
        case (ip, op) => {
          val del = ip._2 - op
          assert(del.abs < 0.1)
          del
        }
      }

      val av = dels.sum / dels.size
      val max = dels.max
      val min = dels.min
      val stddiv = scala.math.pow(
        (dels.map(x => (x - av) * (x - av)).sum / dels.size),
        0.5
      )

      logger.info(s"average error : ${dels.sum / dels.size}")
      logger.info(s"max error     : ${dels.max}")
      logger.info(s"min error     : ${dels.min}")
      logger.info(s"std div       : ${stddiv}")
    }
  }
}

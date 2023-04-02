package fpu

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation
import com.typesafe.scalalogging.LazyLogging
import scala.collection.mutable.ListBuffer
import fpu.util._

class FAddSubSpec
    extends AnyFlatSpec
    with ChiselScalatestTester
    with LazyLogging {
  behavior of "FAddSub"

  val lnOf2 = scala.math.log(2) // natural log of 2
  def log2(x: Double): Double = scala.math.log(x) / lnOf2
  val r = scala.util.Random

  val steps = 15;
  it should "Add two numbers" in {
    for (i <- 0 until 10) {
      test(new FAddSub).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        val a = -100 + 200 * r.nextFloat()
        val b = -100 + 200 * r.nextFloat()
        val diff = r.nextBoolean()
        val s = if (diff) { a - b }
        else { a + b }

        c.io.a.poke(FloatingPoint.make(a))
        c.io.b.poke(FloatingPoint.make(b))
        c.io.o.poke(diff.B)

        c.clock.step(steps)

        val res = FloatingPoint.open(c.io.out.peek())
        val del = s - res
        // println(s"$a ${if (diff) "-" else "+"} $b = $s =?= $res | $del | ${res/s}")
        assert(del.abs < 0.0001)
      }
    }
  }

  val n = 5
  it should s"Add $n pipelined numbers" in {
    test(new FAddSub).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val ips = for (i <- 1 to n) yield {
        val a = -100 + 200 * r.nextFloat()
        val b = -100 + 200 * r.nextFloat()
        val diff = r.nextBoolean()
        val s = if (diff) { a - b }
        else { a + b }
        (a, b, diff, s)
      }

      val outBuffer = new ListBuffer[Float]()
      for (ip <- ips) {
        c.io.a.poke(FloatingPoint.make(ip._1))
        c.io.b.poke(FloatingPoint.make(ip._2))
        c.io.o.poke(ip._3.B)
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
          // logger.info(s"${ip._4} :=: $op")
          val del = ip._4 - op
          assert(del.abs < 0.0001)
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

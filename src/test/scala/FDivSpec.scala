package fpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation
import scala.collection.mutable.ListBuffer
import org.scalatest.matchers.should.Matchers._
import com.typesafe.scalalogging.LazyLogging

class FDivSpec extends AnyFlatSpec with ChiselScalatestTester with LazyLogging {
  behavior of "FDiv"

  val r = scala.util.Random

  val steps = 25
  it should "Divide two numbers" in {
    for (i <- 0 until 10) {
      test(new FDiv).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        val a = -1 + 2 * r.nextFloat()
        val b = -1 + 2 * r.nextFloat()
        val q = a / b

        c.io.a.poke(FloatingPoint.make(a))
        c.io.b.poke(FloatingPoint.make(b))
        c.clock.step(steps)

        val res = FloatingPoint.open(c.io.out.peek())
        val del = q - res

        // println(s"$q =/= ${res} ..... ${res/q}")
        assert(del.abs < 0.0001)
      }
    }
  }

  val n = 5
  it should s"Divide $n pipelined numbers" in {
    test(new FDiv).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val ips = for (i <- 1 to n) yield {
        val a = -10 + 20 * r.nextFloat()
        val b = -10 + 20 * r.nextFloat()
        val q = a / b
        (a, b, q)
      }

      val outBuffer = new ListBuffer[Float]()
      for (ip <- ips) {
        c.io.a.poke(FloatingPoint.make(ip._1))
        c.io.b.poke(FloatingPoint.make(ip._2))
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
          val del = ip._3 - op
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

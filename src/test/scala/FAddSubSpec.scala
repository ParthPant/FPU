package fpu

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation
import com.typesafe.scalalogging.LazyLogging
import scala.collection.mutable.ListBuffer

class FAddSubSpec extends AnyFlatSpec with ChiselScalatestTester with LazyLogging {
    behavior of "FAddSub"

    val lnOf2 = scala.math.log(2) // natural log of 2
    def log2(x: Double): Double = scala.math.log(x) / lnOf2
    val r = scala.util.Random
    
    it should "Add two numbers" in {
        val dels = new ListBuffer[Float]()
        for (i <- 0 until 50) {
          test (new FAddSub).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
              val a = -1 + 2 * r.nextFloat()
              val b = -1 + 2 * r.nextFloat()
              val diff = r.nextBoolean()
              val s = if (diff) { a - b } else { a + b }

              c.io.a.poke(FloatingPoint.make(a))
              c.io.b.poke(FloatingPoint.make(b))
              c.io.o.poke(diff.B)

              c.clock.step(3 + log2(24/4).toInt + 10)

              val res = FloatingPoint.open(c.io.out.peek())
              val del = s - res
              dels += del
              // logger.info(s"[$a ${if (diff) "-" else "+"} $b = $s :=: $res]")
              assert(del.abs < 0.00001)
          }
        }
        val av = dels.sum / dels.size
        val max = dels.max
        val min = dels.min
        val stddiv = scala.math.pow((dels.map(x => (x - av)*(x-av)).sum / dels.size), 0.5)

        logger.info(s"average error : ${dels.sum / dels.size}")
        logger.info(s"max error     : ${dels.max}")
        logger.info(s"min error     : ${dels.min}")
        logger.info(s"std div       : ${stddiv}")
    }
}

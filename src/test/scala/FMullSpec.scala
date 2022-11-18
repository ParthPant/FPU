package fpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation
import scala.collection.mutable.ListBuffer
import org.scalatest.matchers.should.Matchers._
import com.typesafe.scalalogging.LazyLogging

class FMulSpec extends AnyFlatSpec with ChiselScalatestTester with LazyLogging {
    behavior of "FMul" 

    val r = scala.util.Random

    it should "Multiply two numbers" in {
        for (i <- 0 until 50) {
            test (new FMul).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
                val a = -1 + 2 * r.nextFloat()
                val b = -1 + 2 * r.nextFloat()
                val p = a * b

                c.io.a.poke(FloatingPoint.make(a))
                c.io.b.poke(FloatingPoint.make(b))
                c.clock.step(8)

                val res = FloatingPoint.open(c.io.out.peek())
                val del = p - res
                assert(del.abs < 0.00001)
            }
        }
    }

    val n = 20 
    val steps = 8
    it should s"Multiply $n pipelined numbers" in {
        test (new FMul).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
            val ips = for (i <- 1 to n) yield { 
                val a = -1 + 2 * r.nextFloat() 
                val b = -1 + 2 * r.nextFloat()  
                val prod = a * b
                (a, b, prod)
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
            val dels = ips.zip(outList).map{ case (ip, op) => {
                val del = ip._3 - op
                assert(del.abs < 0.00001)
                del
            }}

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
}

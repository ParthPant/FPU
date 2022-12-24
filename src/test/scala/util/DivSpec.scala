package fpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation
import scala.collection.mutable.ListBuffer
import org.scalatest.matchers.should.Matchers._

class ArrayDividerSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ArrayDivider"

  val r = scala.util.Random
  val n =20 
  
  for (w <- List(8, 16, 24, 32)) {
    val hi = BigInt(2).pow(w)
    val stages = Seq(w/3, 2*w/3)
    val steps = stages.size
    it should s"Divide $n $w-bit numbers in ${steps+1} cycles" in {
        test (new ArrayDivider(w, stages)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
          val ips = for (i <- 1 to n) yield { 
            // val a = BigInt(w, r)
            // val b = BigInt(w, r)
            // val (zhi, d) = (a min b, a max b)
            // val zlo = BigInt(w, r)
            // val z = (zhi<<w)|zlo
            val z = BigInt(w, r)
            val d = BigInt(w, r)
            val Q = z/d
            (z, d, Q)
          }

          val outBuffer = new ListBuffer[BigInt]()
          for (ip <- ips) {
              c.io.z.poke(ip._1.U)
              c.io.d.poke(ip._2.U)
              val o = c.io.Q.peek().litValue
              outBuffer += o
              c.clock.step(1)
          }

          for (i <- 1 to steps) {
              val o = c.io.Q.peek().litValue
              outBuffer += o
              c.clock.step(1)
          }
          val outList = outBuffer.toList.drop(outBuffer.size - n)
          ips.zip(outList).map{ case (ip, op) => ip._3 shouldBe op }
      }
    }
  }
}

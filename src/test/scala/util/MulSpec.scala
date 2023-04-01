package fpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import chiseltest.simulator.WriteVcdAnnotation
import scala.collection.mutable.ListBuffer

class ArrayMultiplierSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ArrayMultiplier"

  val r = scala.util.Random
  val n = 5

  for (w <- List(8, 16, 24, 32)) {
    val stages = List(w / 3, 2 * w / 3)
    val steps = stages.size + 1
    val hi = BigInt(2).pow(w)
    it should s"Multiply $n $w-bit numbers in ${steps + 1} cycles" in {
      test(new ArrayMultiplier(w, stages))
        .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
          val ips = for (i <- 1 to n) yield {
            val a = BigInt(w, r)
            val b = BigInt(w, r)
            val prod = a * b
            (a, b, prod)
          }

          val outBuffer = new ListBuffer[BigInt]()
          for (ip <- ips) {
            c.io.a.poke(ip._1.U)
            c.io.b.poke(ip._2.U)
            val o = c.io.P.peek().litValue
            outBuffer += o
            c.clock.step(1)
          }

          for (i <- 1 to steps) {
            val o = c.io.P.peek().litValue
            outBuffer += o
            c.clock.step(1)
          }

          val outList = outBuffer.toList.drop(outBuffer.size - n)
          ips.zip(outList).map { case (ip, op) => ip._3 shouldBe op }
        }
    }
  }
}

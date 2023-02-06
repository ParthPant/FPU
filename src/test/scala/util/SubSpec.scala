package fpu

import chisel3._
import chiseltest._
import scala.math.{pow, log}
import scala.collection.mutable.ListBuffer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import chiseltest.simulator.WriteVcdAnnotation

class FastSubtractorPipelinedSpec
    extends AnyFlatSpec
    with ChiselScalatestTester {
  behavior of "FastSbutractorPipelined"

  val lnOf2 = scala.math.log(2) // natural log of 2
  def log2(x: Double): Double = scala.math.log(x) / lnOf2
  val r = scala.util.Random

  for (w <- List(8, 16, 32, 64)) {
    it should s"Subtract $w-bit numbers" in {
      for (i <- 1 to 5) {
        val hi = BigInt(2).pow(w)
        test(new FastSubtractorPipelined(w, 4))
          .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
            val a = BigInt(w, r)
            val b = BigInt(w, r)
            val cin = r.nextInt(2)
            val d = a - b - cin
            val cout = if (d < 0) 1 else 0
            val diff = if (d < 0) { hi - d.abs }
            else { d }
            val steps = 1 + log2(w / 4).toInt

            c.io.a.poke(a.U)
            c.io.b.poke(b.U)
            c.io.cin.poke(cin.U)

            c.clock.step(steps)

            c.io.Diff.expect(diff.U)
            c.io.Cout.expect(cout.U)
          }
      }
    }
  }

  val n = 5
  for (w <- List(8, 16, 32, 64)) {
    it should s"Subtract $n pipelined $w-bit numbers" in {
      val hi = BigInt(2).pow(w)
      test(new FastSubtractorPipelined(w, 4))
        .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
          val steps = 1 + log2(w / 4).toInt

          val ips = for (i <- 1 to n) yield {
            val a = BigInt(w, r)
            val b = BigInt(w, r)
            val cin = r.nextInt(2)
            val d = a - b - cin
            val cout = if (d < 0) 1 else 0
            val diff = if (d < 0) { hi - d.abs }
            else { d }
            (a, b, cin, diff, cout)
          }

          val outBuffer = new ListBuffer[(BigInt, BigInt)]()
          for (ip <- ips) {
            c.io.a.poke(ip._1.U)
            c.io.b.poke(ip._2.U)
            c.io.cin.poke(ip._3.U)
            val o = (c.io.Diff.peek().litValue, c.io.Cout.peek().litValue)
            outBuffer += o
            c.clock.step(1)
          }

          for (i <- 1 to steps) {
            val o = (c.io.Diff.peek().litValue, c.io.Cout.peek().litValue)
            outBuffer += o
            c.clock.step(1)
          }

          val outList = outBuffer.toList.drop(outBuffer.size - n)
          ips.zip(outList).map { case (ip, op) =>
            (ip._4, ip._5) shouldBe (op._1, op._2)
          }
        }
    }
  }
}

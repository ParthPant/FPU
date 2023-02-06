package fpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation
import scala.collection.mutable.ListBuffer
import org.scalatest.matchers.should.Matchers._

class SqRooterSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "SqRooter"

  def sqrt(n: BigInt): BigDecimal = {
    val d = BigDecimal(n)
    var a = BigDecimal(1.0)
    var b = d
    while (b - a >= 0) {
      val mid = (a + b) / 2
      if (mid * mid - d > 0) b = mid - 0.0001 // adjust down
      else a = mid + 0.0001 // adjust up
    }
    b
  }

  val r = scala.util.Random
  val n = 20

  val w = 8
  it should s"Calculate Square Root of a $w-bit number" in {
    for (ops <- 1 until n) {
      val stages = Seq(1)
      val steps = stages.size
      test(new SqRooter(w, stages)).withAnnotations(Seq(WriteVcdAnnotation)) {
        c =>
          val z = BigInt(w, r)
          val R = sqrt(z)

          c.io.z.poke(z.U)
          c.clock.step(steps)

          val res = c.io.Q.peek().litValue
          // println(s"$z ==> $R =/= $res")
          val del = R - BigDecimal(res)
          assert(del.abs < 1)
      }
    }
  }

  for (w <- Seq(8, 16, 24, 32)) {
    it should s"Calculate Square Root of $n $w-bit pipelined numbers" in {
      val stages = Seq(w / 6, w / 3)
      val steps = stages.size
      test(new SqRooter(w, stages)).withAnnotations(Seq(WriteVcdAnnotation)) {
        c =>
          val ips = for (i <- 1 to n) yield {
            val z = BigInt(w, r)
            val R = sqrt(z)
            (z, R)
          }

          val outBuffer = new ListBuffer[(BigInt, BigInt)]()
          for (ip <- ips) {
            c.io.z.poke(ip._1.U)
            val o = (c.io.Q.peek().litValue, c.io.S.peek().litValue)
            outBuffer += o
            c.clock.step(1)
          }

          for (i <- 1 to steps) {
            val o = (c.io.Q.peek().litValue, c.io.S.peek().litValue)
            outBuffer += o
            c.clock.step(1)
          }
          val outList = outBuffer.toList.drop(outBuffer.size - n)

          ips.zip(outList).map {
            case (ip, op) => {
              val del = ip._2 - BigDecimal(op._1)
              assert(del.abs < 1)
            }
          }
      }
    }
  }
}

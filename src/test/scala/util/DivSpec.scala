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
  val n = 10 
  
  for (w <- List(8, 16, 24, 32)) {
    val stages = Seq(w/3, 2*w/3)
    val steps = stages.size
    it should s"Divide $n $w-bit numbers in ${steps+1} cycles" in {
        test (new ArrayDivider(w, stages)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
          val ips = for (i <- 1 to n) yield { 
            val a = BigInt(w, r)
            val b = BigInt(w, r)
            val zlo = BigInt(w, r)
            val (zhi, d) = (a min b, a max b)
            val z = zhi << w | zlo
            val Q = z/d
            val S = z%d
            (z, d, Q, S)
          }

          val outBuffer = new ListBuffer[(BigInt, BigInt)]()
          for (ip <- ips) {
            c.io.z.poke(ip._1.U)
            c.io.d.poke(ip._2.U)
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
          // outList.map{o => println(s"${o._1.toString(2).reverse.padTo(w, '0').reverse}, ${o._2.toString(2).reverse.padTo(w, '0').reverse}")}
          // ips.zip(outList).map{ case (ip, op) => println(s"${ip._3}/${ip._4} == ${op._1}/${op._2}")}
          ips.zip(outList).map{ case (ip, op) => ip._3 shouldBe op._1}
      }
    }
  }
}

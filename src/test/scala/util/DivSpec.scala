package fpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation
import scala.collection.mutable.ListBuffer
import org.scalatest.matchers.should.Matchers._

class NonRestoringArrayDividerSpec
    extends AnyFlatSpec
    with ChiselScalatestTester {
  behavior of "NonRestoringArrayDivider"

  val r = scala.util.Random
  val n = 10

  val w = 24
  val stages = Seq(w / 3, 2 * w / 3)
  it should s"Divide two $w-bit numbers" in {
    for (ops <- 1 until 10) {
      test(new NonRestoringArrayDivider(w, stages))
        .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
          val d = BigInt(w, r) | BigInt(1 << (w - 1))
          val z = BigInt(w, r) | BigInt(1 << (w - 1))
          val Q = z / d
          val S = z % d

          c.io.z.poke(z.U)
          c.io.d.poke(d.U)

          c.clock.step(3)

          // println(s"$z/$d = $Q =/= ${c.io.Q.peek().litValue}")

          c.io.Q.expect(Q.U)
        }
    }
  }

  for (w <- List(8, 16, 24, 32)) {
    val stages = Seq(w / 3, 2 * w / 3)
    val steps = stages.size
    it should s"Divide $n $w-bit numbers in ${steps + 1} cycles" in {
      test(new NonRestoringArrayDivider(w, stages))
        .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
          val ips = for (i <- 1 to n) yield {
            val a = BigInt(w, r)
            val b = BigInt(w, r)
            val zlo = BigInt(w, r)
            val (zhi, d) = (a min b, a max b)
            val z = zhi << w | zlo
            val Q = z / d
            val S = z % d
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
          ips.zip(outList).map { case (ip, op) => ip._3 shouldBe op._1 }
        }
    }
  }
}

class RestoringArrayDividerSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "RestoringArrayDivider"

  val r = scala.util.Random
  val n = 10

  val w = 24
  it should "Divide two 4-bit numbers" in {
    val stages = Seq(w / 3, 2 * w / 3)
    for (ops <- 1 until 30) {
      test(new RestoringArrayDivider(w, stages))
        .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
          val d = BigInt(w, r) | BigInt(1 << (w - 1))
          val z = (BigInt(w, r) | BigInt(1 << (w - 1))) << (w - 1)
          val Q = z / d
          val S = z % d

          c.io.z.poke(z.U)
          c.io.d.poke(d.U)

          c.clock.step(3)
          c.io.Q.expect(Q.U)
        }
    }
  }

  for (w <- List(8, 16, 24, 32)) {
    val stages = Seq(w / 3, 2 * w / 3)
    val steps = stages.size
    it should s"Divide $n $w-bit numbers in ${steps + 1} cycles" in {
      test(new RestoringArrayDivider(w, stages))
        .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
          val ips = for (i <- 1 to n) yield {
            val a = BigInt(w, r)
            val b = BigInt(w, r)
            val zlo = BigInt(w, r)
            val (zhi, d) = (a min b, a max b)
            val z = zhi << w | zlo
            val Q = z / d
            val S = z % d
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
          // ips.zip(outList).map{ case (ip, op) => println(s"${ip._3}/${ip._4} =/= ${op._1}/${op._2}")}
          ips.zip(outList).map {
            case (ip, op) => {
              ip._3 shouldBe op._1
              ip._4 shouldBe op._2
            }
          }
        }
    }
  }

  it should s"Divide shifted floating numbers" in {
    for (i <- 0 until 10) {
      val stages = Seq(w / 3, 2 * w / 3)
      test(new RestoringArrayDivider(24, stages)) { c =>
        val z = (BigInt(w, r) | BigInt(1 << (w - 1))) << (w - 1)
        val d = BigInt(w, r) | BigInt(1 << (w - 1))
        val Q = z / d
        val S = z % d

        c.io.z.poke(z.U)
        c.io.d.poke(d.U)

        c.clock.step(3)

        val qs =
          c.io.Q.peek().litValue.toString(2).reverse.padTo(24, '0').reverse
        // println(s"$z/$d == $Q/$S =/= ${c.io.Q.peek().litValue}/${c.io.S.peek().litValue}")
        // println(s"Q = $qs")

        c.io.Q.expect(Q.U)
      }
    }
  }
}

package fpu.util.subscalar

import chisel3._
import chiseltest._
import fpu.util._
import chiseltest.ChiselScalatestTester
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation

class AddTest(val width: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val cin = Input(Bool())

    val s = Output(UInt(width.W))
    val cout = Output(Bool())
  })

  val a = Wire(Vec(width, Bool()))
  val b = Wire(Vec(width, Bool()))
  for ((i, d) <- (0 until width by 2).zipWithIndex) {
    a(i) := Delay(io.a(i), d)
    a(i+1) := Delay(io.a(i+1), d)

    b(i) := Delay(io.b(i), d)
    b(i+1) := Delay(io.b(i+1), d)
  }

  val adder = Module(new Adder(width))
  adder.io.a := a.asUInt
  adder.io.b := b.asUInt
  adder.io.cin := io.cin

  val out = Wire(Vec(width, Bool()))
  for ((i, d) <- (0 until width by 2).zipWithIndex) {
    out(i) := Delay(adder.io.s(i), width/2 - d)
    out(i+1) := Delay(adder.io.s(i+1), width/2 - d)
  }

  io.s := out.asUInt
  // io.s := adder.io.s
  io.cout := adder.io.cout
}

class AddSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "SubscalarAdder"

  val r = scala.util.Random

  val w = 8
  val hi = BigInt(2).pow(w)

  it should s"add 2 numbers subscalar" in {
    for (i <- 0 until 10) {
      test(new AddTest(w)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        val a = BigInt(w, r)
        val b = BigInt(w, r)
        val cin = r.nextInt(2)
        val cout = if (a + b + cin >= hi) 1 else 0
        val sum = (a + b + cin) & (hi - 1)

        c.io.a.poke(a.U)
        c.io.b.poke(b.U)
        c.io.cin.poke(cin.U)

        c.clock.step(w/2 + 1)

        c.io.s.expect(sum.U)
        c.io.cout.expect(cout.U)
      }
    }
  }
}

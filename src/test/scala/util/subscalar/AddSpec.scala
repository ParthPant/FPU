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
  for (i <- 0 until width) {
    a(i) := Delay(io.a(i), i) // (if (i == 0) io.a(i) else Delay(io.a(i), i))
    b(i) := Delay(io.b(i), i) // (if (i == 0) io.b(i) else Delay(io.b(i), i))
  }

  val adder = Module(new SSAdder(width))
  adder.io.a := a.asUInt
  adder.io.b := b.asUInt
  adder.io.cin := io.cin

  val out = Wire(Vec(width, Bool()))
  for (i <- 0 until width) {
    out(i) := Delay(adder.io.s(i), width - i)
  }

  io.s := out.asUInt()
  io.cout := adder.io.cout
}

class SSAddSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "SSAdder"

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

        c.clock.step(w + 1)

        c.io.s.expect(sum.U)
        c.io.cout.expect(cout.U)
      }
    }
  }
}

package fpu.util.subscalar

import chisel3._
import chiseltest._
import fpu.util._
import chiseltest.ChiselScalatestTester
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation

class MuxTest(val width: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val sel = Input(Bool())

    val out = Output(UInt(width.W))
  })

  val a = Wire(Vec(width, Bool()))
  val b = Wire(Vec(width, Bool()))
  for ((i, d) <- (0 until width by 2).zipWithIndex) {
    a(i) := Delay(io.a(i), d) // (if (i == 0) io.a(i) else Delay(io.a(i), i))
    a(i+1) := Delay(io.a(i+1), d) // (if (i == 0) io.a(i) else Delay(io.a(i), i))

    b(i) := Delay(io.b(i), d) // (if (i == 0) io.b(i) else Delay(io.b(i), i))
    b(i+1) := Delay(io.b(i+1), d) // (if (i == 0) io.b(i) else Delay(io.b(i), i))
  }

  val mux = Module(new Mux(width))
  mux.io.a := a.asUInt
  mux.io.b := b.asUInt
  mux.io.sel := io.sel

  val out = Wire(Vec(width, Bool()))
  for ((i, d) <- (0 until width by 2).zipWithIndex) {
    out(i) := Delay(mux.io.out(i), width/2 - d)
    out(i+1) := Delay(mux.io.out(i+1), width/2 - d)
  }

  io.out := out.asUInt()
}

class MuxSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "SubscalarMux"

  val r = scala.util.Random

  val w = 8
  val hi = BigInt(2).pow(w)

  it should s"select between two numbers" in {
    for (i <- 0 until 10) {
      test(new MuxTest(w)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        val a = BigInt(w, r)
        val b = BigInt(w, r)
        val sel = r.nextBoolean()
        val out = if (sel) a else b

        c.io.a.poke(a.U)
        c.io.b.poke(b.U)
        c.io.sel.poke(sel.B)

        c.clock.step(w/2)

        c.io.out.expect(out.U)
      }
    }
  }
}

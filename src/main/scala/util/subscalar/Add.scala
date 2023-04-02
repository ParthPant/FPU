package fpu.util.subscalar

import chisel3._
import fpu.util._

class PAdd extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Input(Bool())
    val cin = Input(Bool())

    val s = Output(Bool())
    val cout = Output(Bool())
  })

  val axorb = io.a ^ io.b

  io.s := axorb ^ io.cin
  io.cout := (io.a & io.b) || (axorb & io.cin)
}

class SSAdder(val width: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val cin = Input(Bool())

    val s = Output(UInt(width.W))
    val cout = Output(Bool())
  })

  val ss = Wire(Vec(width, Bool()))
  val cs = Wire(Vec(width, Bool()))

  for (i <- 0 until width) {
    val padd = Module(new PAdd);
    padd.io.a := io.a(i)
    padd.io.b := io.b(i)
    padd.io.cin := (if (i == 0) io.cin else cs(i - 1))
    ss(i) := padd.io.s
    cs(i) := Delay(padd.io.cout, 1)
  }

  io.s := ss.asUInt
  io.cout := cs.last.asUInt
}

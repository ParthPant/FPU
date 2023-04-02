package fpu.util.subscalar

import chisel3._
import fpu.util._

class SSMux(val width: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val sel = Input(Bool())

    val out = Output(UInt(width.W))
  })

  val outs = Wire(Vec(width, Bool()))

  for (i <- 0 until width) {
    outs(i) := Mux(Delay(io.sel, i), io.a(i), io.b(i))
  }

  io.out := outs.asUInt()
}

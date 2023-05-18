package fpu.util.subscalar

import chisel3._
import fpu.util._

class Pmlt extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(2.W))
    val b = Input(UInt(2.W))
    val c = Input(UInt(2.W))

    val x = Output(UInt(2.W))
    val y = Output(UInt(2.W))
  })

  val p = io.a * io.b + io.c
  io.x := RegNext(p(1, 0))
  io.y := RegNext(p(2, 1))
}

class Mul(val width: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val p = Input(UInt((2*width).W))
  })
}

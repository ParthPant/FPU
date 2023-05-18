package fpu.util.subscalar

import chisel3._
import chisel3.util._
import fpu.util._

class Padd extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(2.W))
    val b = Input(UInt(2.W))
    val c = Input(UInt(1.W))

    val x = Output(UInt(2.W))
    val y = Output(UInt(1.W))
  })

  val sum = io.a +& io.b +& io.c
  io.x := RegNext(sum(1, 0))
  io.y := RegNext(sum(2).asUInt)
}

class Adder(val width: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val cin = Input(Bool())

    val s = Output(UInt(width.W))
    val cout = Output(Bool())
  })

  val ss = Wire(Vec(width, UInt(1.W)))
  val cs = Wire(Vec(width/2, UInt(1.W)))

  for ((i, d) <- (0 until width by 2).zipWithIndex) {
    val padd = Module(new Padd);
    padd.io.a := io.a(i + 1, i)
    padd.io.b := io.b(i + 1, i)
    padd.io.c := (if (d == 0) io.cin else cs(d - 1))
    ss(i) := padd.io.x(0)
    ss(i + 1) := padd.io.x(1)
    cs(d) := padd.io.y
  }

  io.s := ss.asUInt
  io.cout := cs.last.asUInt
}

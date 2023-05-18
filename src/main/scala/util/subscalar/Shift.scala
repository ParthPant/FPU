package fpu.util.subscalar

import chisel3._
import chisel3.util._
import fpu.util._

class _shift extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(2.W))
    val b = Input(UInt(2.W))
    val c = Input(UInt(2.W))

    val x = Output(UInt(2.W))
    val y = Output(UInt(2.W))
  })

  val v = Cat(0.U(2.W), io.b)
  val out = v << io.c

  io.x := Delay(out, 1)(1, 0)
  io.y := Delay(out, 1)(3, 2)
}

class or extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(2.W))
    val b = Input(UInt(2.W))

    val x = Input(UInt(2.W))
  })

  io.x := Delay(io.a | io.b, 1)
}

class _mux extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(2.W))
    val b = Input(UInt(2.W))
    val c = Input(UInt(1.W))

    val x = Output(UInt(2.W))
    val y = Output(UInt(1.W))
  })

  io.x := Delay(Mux(io.c.asBool, io.a, io.b), 1)
  io.y := Delay(io.c, 1)
}


class Shift(val width: Int) extends Module {
  val shiftw = log2Up(width)
  val io = IO(new Bundle {
    val in = Input(UInt(width.W))
    val shamt = Input(UInt(shiftw.W))

    val out = Output(UInt(width.W))
  })

  val sel = Delay(io.shamt(width - 1), 3)

  val shiftoutxs = Wire(Vec(width/2, UInt(2.W)))
  val shiftoutys = Wire(Vec(width/2, UInt(2.W)))

  for ((i, d) <- (0 until width by 2).zipWithIndex) {
    val shift = Module(new _shift)
    shift.io.a := 0.U
    shift.io.b := io.in(i+1, i)
    shift.io.c := Delay(io.shamt, d)

    shiftoutxs(d) := shift.io.x
    shiftoutys(d) := shift.io.y
  }

  val _orouts = Wire(Vec(width/2 - 1, UInt(2.W)))
  for (i <- 0 until width/2 - 1) {
    val or = Module(new or)
    or.io.a := Delay(shiftoutys(i), 1)
    or.io.b := shiftoutxs(i+1)

    _orouts(i) := or.io.x
  }

  val orouts = Wire(Vec(width, UInt(2.W)))
  orouts.head := Delay(shiftoutxs.head, 1)
  orouts.last := Delay(shiftoutys.last, 1)
  for (i <- 0 until width/2 - 1) {
    orouts(i+1) := _orouts(2*i)
    orouts(i+2) := Delay(_orouts(2*i), 1)
  }

  val muxouts = Wire(Vec(width/2, UInt(2.W)))
  val prevs = Wire(Vec(width/2, UInt(1.W)))
  for ((i, d) <- (0 until width by 2).zipWithIndex) {
    val mux = Module(new _mux)
    mux.io.a := Delay(orouts(i), 1)
    mux.io.b := orouts(i+1)
    mux.io.c := (if (d == 0) sel else prevs(d-1))

    muxouts(i) := mux.io.x
  }

  io.out := Cat(muxouts).asUInt
}

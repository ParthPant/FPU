package fpu

import chisel3._
import chisel3.util._
import scala.math.pow

class FAddSub extends Module {
  val io = IO(new Bundle {
    val a = Input(new FloatingPoint)
    val b = Input(new FloatingPoint)
    val o = Input(Bool())
    val out = Output(new FloatingPoint)
  })

  val a = io.a
  val b = FloatingPoint(io.o ^ io.b.sign, io.b.significand, io.b.exp)
  val agtb = io.a.exp >= io.b.exp

  val _x = Mux(agtb, a, b) // 1
  val _y = Mux(agtb, b, a) // 1

  val y = RegNext(
    FloatingPoint(
      _y.sign,
      ShiftRight(24)(_y.significand, _x.exp - _y.exp),
      _x.exp
    )
  ) // 2
  val x = RegNext(_x) // 2

  val sz = (x.sign ^ y.sign).asBool // 2

  val adder = Module(new FastAdderPipelined(24, 4))
  adder.io.a := x.significand // 2
  adder.io.b := y.significand // 2
  adder.io.cin := 0.U // 2

  val subtractor = Module(new FastAdderPipelined(24, 4))
  subtractor.io.a := x.significand // 2
  subtractor.io.b := ~y.significand // 2
  subtractor.io.cin := 1.U // 2

  val szreg = Delay(sz, 3) // 5
  val xsign = Delay(x.sign, 3) // 5
  val ysign = Delay(y.sign, 3) // 5

  val sum = Mux(szreg, subtractor.io.Sum, adder.io.Sum) // 5
  val cout = Mux(szreg, subtractor.io.Cout, adder.io.Cout) // 5
  val diff = Mux(cout.asBool, sum, ~sum + 1.U) // 5
  val signout = Mux(szreg, xsign ^ ~cout.asBool, xsign) // 5

  val xexp = Delay(x.exp, 4) // 6
  val signoutreg = RegNext(signout) // 6
  val sumreg = RegNext(sum) // 6
  val coutreg = RegNext(cout.asBool) // 6

  when(RegNext(~szreg)) {
    when(coutreg) {
      io.out.significand := Cat(1.U, sumreg(23, 1)) // 6
      io.out.exp := xexp + 1.U // 6
    }.otherwise {
      io.out.significand := sumreg // 6
      io.out.exp := xexp // 6
    }
  }.otherwise {
    val clz = Module(new CLZ24)
    clz.io.in := diff // 5

    val clzreg = RegNext(clz.io.Z) // 6
    val diffreg = RegNext(diff) // 6
    val expout = xexp - clzreg // 6

    val shifter = Module(new ShiftLeft(24))
    shifter.io.a := diffreg // 6
    shifter.io.shift := clzreg // 6

    io.out.significand := shifter.io.out // 6
    io.out.exp := expout // 6
  }

  io.out.sign := signoutreg // 6
}

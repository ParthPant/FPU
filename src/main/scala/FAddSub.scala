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

  val op = io.a.sign.asBool ^ (io.b.sign.asBool ^ io.o)

  val expsub = Module(new FastAdderPipelined(8, 4))
  expsub.io.a := io.a.exp;
  expsub.io.b := ~io.b.exp;
  expsub.io.cin := 1.U;
  val exp_diff = Mux(expsub.io.Cout.asBool, expsub.io.Sum, ~expsub.io.Sum + 1.U)
  val diff_gt = exp_diff > 24.U

  val op1 = Wire(UInt(48.W))
  val op2 = Wire(UInt(48.W))
  val exp = Wire(UInt(8.W))

  when(~diff_gt) {
    op1 := io.a.significand
    op2 := Mux(
      expsub.io.Cout.asBool,
      ShiftRight(48)(io.b.significand, exp_diff), // +ve
      ShiftLeft(48)(io.b.significand, exp_diff)   // -ve
    )
    exp := Mux(expsub.io.Cout.asBool, io.a.exp, io.b.exp)
  }.otherwise {
    //                                +ve               -ve
    op1 := Mux(expsub.io.Cout.asBool, io.a.significand, 0.U)
    op2 := Mux(expsub.io.Cout.asBool, 0.U, io.b.significand)
    exp := Mux(expsub.io.Cout.asBool, io.a.exp, io.b.exp)
  }

  val adder = Module(new FastAdderPipelined(48, 6))
  adder.io.a := op1
  adder.io.b := op2
  adder.io.cin := 0.U;

  val subtractor = Module(new FastAdderPipelined(48, 6))
  subtractor.io.a := op1
  subtractor.io.b := ~op2
  subtractor.io.cin := 1.U;
  val subout =
    Mux(subtractor.io.Cout.asBool, subtractor.io.Sum, ~subtractor.io.Sum + 1.U)

  val sumout = Mux(op, subout, adder.io.Sum)
  val cout = Mux(op, subtractor.io.Cout, adder.io.Cout)

  val clz = Module(new CLZ48())
  clz.io.in := sumout
  val lzs = clz.io.Z

  val sum = ShiftLeft(48)(sumout, lzs)(47, 25)
  val expout = Wire(UInt(8.W))
  when(~diff_gt) {
    val off = Mux(expsub.io.Cout.asBool, lzs - 24.U, 24.U - lzs)
    expout := exp + off
  }.otherwise {
    expout := exp
  }
  val signout = io.a.sign

  io.out.sign := signout
  io.out.exp := expout
  io.out.significand := sum
}

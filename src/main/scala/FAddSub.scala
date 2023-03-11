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

  // 1
  val op = io.a.sign.asBool ^ (io.b.sign.asBool ^ io.o)
  val asig = io.a.significand
  val bsig = io.b.significand
  val aexp = io.a.exp
  val bexp = io.b.exp
  val asign = io.a.sign

  val expsub = Module(new FastAdderPipelined(8, 4))
  expsub.io.a := io.a.exp;
  expsub.io.b := ~io.b.exp;
  expsub.io.cin := 1.U;
  // 3
  val exp_diff = Mux(expsub.io.Cout.asBool, expsub.io.Sum, ~expsub.io.Sum + 1.U)
  val exp_cout = expsub.io.Cout.asBool
  val diff_gt = exp_diff > 24.U

  val op1 = Wire(UInt(48.W))
  val op2 = Wire(UInt(48.W))
  val exp = Wire(UInt(8.W))

  val bsigreg = Delay(bsig, 2)
  val asigreg = Delay(asig, 2)
  val aexpreg = Delay(aexp, 2)
  val bexpreg = Delay(bexp, 2)

  when(~diff_gt) {
    op1 := asigreg
    op2 := Mux(
      exp_cout,
      ShiftRight(48)(bsigreg, exp_diff),
      ShiftLeft(48)(bsigreg, exp_diff)
    )
    exp := aexpreg
  }.otherwise {
    op1 := Mux(exp_cout, Cat(asigreg, 0.U(24.W)), 0.U)
    op2 := Mux(exp_cout, 0.U, Cat(bsigreg, 0.U(24.W)))
    exp := Mux(exp_cout, aexpreg, bexpreg)
  }

  val adder = Module(new FastAdderPipelined(48, 6))
  adder.io.a := op1
  adder.io.b := op2
  adder.io.cin := 0.U;

  val subtractor = Module(new FastAdderPipelined(48, 6))
  subtractor.io.a := op1
  subtractor.io.b := ~op2
  subtractor.io.cin := 1.U;

  // 7
  val addout = adder.io.Sum
  val subout =
    Mux(subtractor.io.Cout.asBool, subtractor.io.Sum, ~subtractor.io.Sum + 1.U)
  val sumout = Mux(Delay(op, 6), subout, addout)
  val cout = Mux(Delay(op, 6), subtractor.io.Cout, adder.io.Cout)
  val asignreg = Delay(asign, 6)
  val signout = Mux(Delay(op, 6), asignreg ^ ~cout, asignreg)

  // 8
  val clz = Module(new CLZ48())
  val sumval = Delay(sumout, 1)
  val expval = Delay(exp, 5)
  clz.io.in := sumval
  val lzs = clz.io.Z
  val sum = ShiftLeft(48)(sumval, lzs)(47, 24)
  when(diff_gt) {
    io.out.exp := expval
  }.otherwise {
    val expsel = lzs < 24.U
    val off = Mux(expsel, 24.U - lzs, lzs - 24.U)
    io.out.exp := Mux(expsel, expval + off, expval - off)
  }
  io.out.sign := Delay(signout, 1)
  io.out.significand := sum
}

package fpu

import chisel3.util._
import chisel3._
import fpu.util._

class FRoot extends Module {
  val io = IO(new Bundle {
    val a = Input(new FloatingPoint)
    val out = Output(new FloatingPoint)
  })

  val even = io.a.exp(0)
  val neg = ~io.a.exp(7)

  val diff = VecInit
    .tabulate(8)(i => if (i == 7) ~io.a.exp(i) else io.a.exp(i))
    .asUInt
  val subout = Delay(Mux(neg, ~diff, diff - 1.U), 1)

  val rootin = Mux(even, io.a.significand >> 1, io.a.significand)
  val rooter = Module(new SqRooter(24, Seq(), true))
  rooter.io.z := rootin
  val rootout = rooter.io.Q

  val expout = Wire(UInt(8.W))
  when(Delay(neg, 1)) {
    val exp = Mux(subout(0), subout - 1.U, subout) >> 1.U
    expout := VecInit
      .tabulate(8)(i => if (i == 7) exp(i) else ~exp(i))
      .asUInt
  }.otherwise {
    val exp = Mux(subout(0), subout + 1.U, subout) >> 1.U
    expout := VecInit
      .tabulate(8)(i => if (i == 7) ~exp(i) else exp(i))
      .asUInt
  }

  val expres = Delay(expout, 12)
  io.out.exp := Mux(Delay(even, 13), expres, expres - 1.U)
  io.out.sign := 0.U
  io.out.significand := Cat(rootout, 0.U(12.W))
}

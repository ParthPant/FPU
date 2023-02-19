package fpu

import chisel3.util._
import chisel3._

class FRoot extends Module {
  val io = IO(new Bundle {
    val a = Input(new FloatingPoint)
    val out = Output(new FloatingPoint)
  })

  val subtractor = Module(new FastSubtractorPipelined(8, 4))
  subtractor.io.a := io.a.exp
  subtractor.io.b := 127.U
  subtractor.io.cin := 0.U
  val subout = Mux(subtractor.io.Cout.asBool, ~subtractor.io.Diff + 1.U, subtractor.io.Diff)

  val rooter = Module(new SqRooter(24, Seq(6)))
  rooter.io.z := Mux(io.a.exp(0), io.a.significand, io.a.significand >> 2)
  val rootout = rooter.io.Q

  val exp = Mux(subout(0), (subout + 1.U) >> 1.U, (subout >> 1.U) + 1.U)
  val off = Mux(rootout(11), (127-1).U, (127-2).U)
  val expout = Mux(subtractor.io.Cout.asBool, off - exp, off + exp)

  val sigout = Mux(rootout(11), Cat(rootout, 0.U(12.W)), Cat(rootout(10, 0), 0.U(13.W))) 

  io.out.sign := io.a.sign
  io.out.exp := expout
  io.out.significand := sigout
}

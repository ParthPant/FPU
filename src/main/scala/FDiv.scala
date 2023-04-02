package fpu

import chisel3._
import fpu.util._

class FDiv extends Module {
  val io = IO(new Bundle {
    val a = Input(new FloatingPoint)
    val b = Input(new FloatingPoint)

    val out = Output(new FloatingPoint)
  })

  val signout = Delay(io.a.sign ^ io.b.sign, 25)

  val divider = Module(new RestoringArrayDivider(24, List(), true))
  divider.io.z := io.a.significand << 23
  divider.io.d := io.b.significand
  val divout = divider.io.Q

  val subtractor = Module(new FastAdderPipelined(8, 4))
  subtractor.io.a := io.a.exp
  subtractor.io.b := ~io.b.exp
  subtractor.io.cin := 0.U
  val expdiff = VecInit
    .tabulate(8)(i =>
      if (i == 7) ~subtractor.io.Sum(i) else subtractor.io.Sum(i)
    )
    .asUInt
  val subout = Delay(expdiff, 22)

  val dec = Module(new FastSubtractorPipelined(8, 4))
  dec.io.a := expdiff
  dec.io.b := 1.U
  dec.io.cin := 0.U
  val suboutdec = Delay(dec.io.Diff, 19)

  val sigout = Mux(divout(23), divout, divout << 1)
  val expout = Mux(divout(23), subout, suboutdec)

  io.out.exp := expout
  io.out.significand := sigout
  io.out.sign := signout
}

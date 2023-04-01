package fpu

import chisel3._

class FMul extends Module {
  val io = IO(new Bundle {
    val a = Input(new FloatingPoint)
    val b = Input(new FloatingPoint)

    val out = Output(new FloatingPoint)
  })

  val signout = Delay(io.a.sign ^ io.b.sign, 25)

  val multiplier = Module(new ArrayMultiplier(24, List(), true))
  multiplier.io.a := io.a.significand
  multiplier.io.b := io.b.significand
  val multout = multiplier.io.P

  val adder = Module(new FastAdderPipelined(8, 4))
  adder.io.a := io.a.exp
  adder.io.b := io.b.exp
  adder.io.cin := 1.U
  val expsum = VecInit
    .tabulate(8)(i => if (i == 7) ~adder.io.Sum(i) else adder.io.Sum(i))
    .asUInt
  val addout = Delay(expsum, 22)

  val inc = Module(new FastAdderPipelined(8, 4))
  inc.io.a := expsum
  inc.io.b := 0.U
  inc.io.cin := 1.U
  val addoutinc = Delay(inc.io.Sum, 19)

  val sigout = Mux(multout(47), multout(47, 24), multout(46, 23))
  val expout = Mux(multout(47), addoutinc, addout)

  io.out.exp := expout
  io.out.significand := sigout
  io.out.sign := signout
}

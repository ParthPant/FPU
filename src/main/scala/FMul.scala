package fpu

import chisel3._
import util._

class FMul extends Module {
    val io = IO(new Bundle {
        val a = Input(new FloatingPoint)
        val b = Input(new FloatingPoint)

        val out = Output(new FloatingPoint)
    }) 

    val signout = Delay(io.a.sign ^ io.b.sign, 2)

    val multiplier = Module(new ArrayMultiplier(24, List(8, 16)))
    multiplier.io.a := io.a.significand
    multiplier.io.b := io.b.significand
    val multout = multiplier.io.P

    val adder = Module(new FastAdderPipelined(8, 4))
    adder.io.a := io.a.exp
    adder.io.b := io.b.exp
    adder.io.cin := 1.U
    val addout = VecInit.tabulate(8)(i => if (i==7) ~adder.io.Sum(i) else adder.io.Sum(i)).asUInt

    val sigout = Mux(multout(47), multout(47,24), multout(46, 23))
    val expout = Mux(multout(47), addout + 1.U, addout)

    io.out.exp := expout
    io.out.significand := sigout
    io.out.sign := signout 
}

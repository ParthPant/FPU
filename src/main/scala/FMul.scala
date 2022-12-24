package fpu

import chisel3._
import util._

class FMul extends Module {
    val io = IO(new Bundle {
        val a = Input(new FloatingPoint)
        val b = Input(new FloatingPoint)

        val out = Output(new FloatingPoint)
    }) 

    val signout = Delay(io.a.sign ^ io.b.sign, 4)

    val multiplier = Module(new ArrayMultiplier(24, List(6, 12, 18)))
    multiplier.io.a := io.a.significand
    multiplier.io.b := io.b.significand
    val multout = RegNext(multiplier.io.P)

    val adder = Module(new FastAdderPipelined(10, 5))
    adder.io.a := io.a.exp
    adder.io.b := io.b.exp
    adder.io.cin := 0.U
    val addout = adder.io.Sum

    val subtractor = Module(new FastSubtractorPipelined(10, 5))
    subtractor.io.a := addout
    subtractor.io.b := 127.U
    subtractor.io.cin := 0.U
    val subout = subtractor.io.Diff

    val sigout = Mux(multout(47), multout(47,24), multout(46, 23))
    val expout = Mux(multout(47), subout + 1.U, subout)

    io.out.exp := expout
    io.out.significand := sigout
    io.out.sign := signout 
}

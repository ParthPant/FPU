package fpu

import chisel3._
import util._

class FDiv extends Module {
    val io = IO(new Bundle {
        val a = Input(new FloatingPoint)
        val b = Input(new FloatingPoint)

        val out = Output(new FloatingPoint)
    }) 

    val signout = Delay(io.a.sign ^ io.b.sign, 4)

    val divider = Module(new ArrayDivider(24, List()))
    divider.io.z := io.a.significand
    divider.io.d := io.b.significand
    val divout = RegNext(divider.io.Q)

    val subtractor = Module(new FastSubtractorPipelined(10, 5))
    subtractor.io.a := io.a.exp
    subtractor.io.b := io.b.exp
    subtractor.io.cin := 0.U
    val subout = subtractor.io.Diff

    val adder = Module(new FastAdderPipelined(10, 5))
    adder.io.a := subout
    adder.io.b := 127.U
    adder.io.cin := 0.U
    val addout = adder.io.Sum

    val sigout = Mux(divout(24), divout, divout)
    val expout = Mux(divout(24), addout + 1.U, addout)

    io.out.exp := expout
    io.out.significand := sigout
    io.out.sign := signout 
}

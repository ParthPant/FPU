package fpu

import chisel3._
import util._

class FMul extends Module {
    val io = IO(new Bundle {
        val a = Input(new FloatingPoint)
        val b = Input(new FloatingPoint)

        val out = Output(new FloatingPoint)
    }) 

    val signout = Delay(io.a.sign ^ io.b.sign, 8)

    val multiplier = Module(new ArrayMultiplier(24, List(6, 12, 18)))
    multiplier.io.a := io.a.significand
    multiplier.io.b := io.b.significand
    val multreg = RegNext(multiplier.io.P) // 4

    val clz = Module(new CLZ48)
    clz.io.in := multreg
    val leadingzreg = RegNext(clz.io.Z) // 5
    val multreg2 = RegNext(multreg) // 5

    val shift = Module(new ShiftLeft(48))
    shift.io.a := multreg2
    shift.io.shift := leadingzreg
    val shiftreg = Delay(shift.io.out, 3) // 8
    val leadingzreg2 = RegNext(leadingzreg) // 6

    val adder = Module(new FastAdderPipelined(12, 4))
    adder.io.a := io.a.exp
    adder.io.b := io.b.exp
    adder.io.cin := 0.U
    val addreg = RegNext(adder.io.Sum) // 3

    val subtractor1 = Module(new FastSubtractorPipelined(12, 4))
    subtractor1.io.a := addreg
    subtractor1.io.b := 126.U
    subtractor1.io.cin := 0.U
    val subreg = RegNext(subtractor1.io.Diff) // 6

    val subtractor2 = Module(new FastSubtractorPipelined(12, 4))
    subtractor2.io.a := subreg
    subtractor2.io.b := leadingzreg2
    subtractor2.io.cin := 0.U
    
    val shiftout = shiftreg
    val subout = subtractor2.io.Diff
    
    io.out.exp := subout
    io.out.significand := shiftout(47, 24)
    io.out.sign := signout 
}

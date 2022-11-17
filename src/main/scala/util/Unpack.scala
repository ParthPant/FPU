package fpu

import chisel3._
import chisel3.util._

class Unpack32 extends Module {
    val io = IO(new Bundle {
        val in = Input(UInt(32.W))
        val out = Output(new FloatingPoint)
    })

    val ins = io.in.asBools
    val out = Wire(new FloatingPoint)

    out.sign := ins(31)
    out.exp :=  VecInit(ins.slice(23, 32)).asUInt
    out.significand := Cat(1.U, VecInit(ins.slice(0, 23)).asUInt)

    io.out := out
}

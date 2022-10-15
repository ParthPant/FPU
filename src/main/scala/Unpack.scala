package fpu

import chisel3._
import chisel3.util._

class Unpack32 extends Module {
    val io = IO(new Bundle {
        val in = Input(UInt(32.W))
        val s = Output(UInt(1.W))
        val e = Output(UInt(8.W))
        val f = Output(UInt(24.W))
    })

    val ins = io.in.asBools

    io.s := ins(31)
    io.e :=  VecInit(ins.slice(23, 32)).asUInt
    io.f := Cat(1.U, VecInit(ins.slice(0, 23)).asUInt)
}
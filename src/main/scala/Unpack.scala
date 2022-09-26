package fpu

import chisel3._

class Unpack32 extends Module {
    val io = IO(new Bundle {
        val in = Input(UInt(32.W))
        val s = Output(UInt(1.W))
        val e = Output(UInt(8.W))
        val f = Output(UInt(23.W))
    })

    io.s := io.in(31)

    val exp = Wire(Vec(8, UInt(1.W)))
    val significand = Wire(Vec(23, UInt(1.W)))

    for (i <- 0 until 8) {
       exp(i) :=  io.in(23+i)
    }
    for (i <- 0 until 23) {
       significand(i) := io.in(i)
    }

    io.e := exp.asUInt
    io.f := significand.asUInt
}

class Unpack64 extends Module {
    val io = IO(new Bundle {
        val in = Input(UInt(64.W))
        val s = Output(UInt(1.W))
        val e = Output(UInt(11.W))
        val f = Output(UInt(52.W))
    })

    io.s := io.in(63)

    val exp = Wire(Vec(11, UInt(1.W)))
    val significand = Wire(Vec(52, UInt(1.W)))

    for (i <- 0 until 11) {
       exp(i) :=  io.in(52+i)
    }
    for (i <- 0 until 52) {
       significand(i) := io.in(i)
    }

    io.e := exp.asUInt
    io.f := significand.asUInt
}
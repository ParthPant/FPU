package fpu

import chisel3._

class FourToTwoReducer(val width: Int) extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(width.W))
        val b = Input(UInt(width.W))
        val c = Input(UInt(width.W))
        val d = Input(UInt(width.W))

        val S = Output(UInt(width.W))
        val C = Output(UInt(width.W))
    })

    val (s1, c1) = CarrySaveAdder(width)(io.a, io.b, io.c)
    val (s2, c2) = CarrySaveAdder(width)(s1, c1, io.d)

    io.S := s2
    io.C := c2
}

class Multiplier(val width: Int) extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(width.W))
        val x = Input(UInt(width.W))

        val p = Output(UInt((2*width).W))
        val cout = Output(UInt(1.W))
    })
}
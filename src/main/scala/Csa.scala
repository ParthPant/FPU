package fpu

import chisel3._

class FA extends Module {
    val io = IO(new Bundle {
        val a = Input(Bool())
        val b = Input(Bool())
        val cin = Input(Bool())

        val s = Output(Bool())
        val cout = Output(Bool())
    })

    val axorb = io.a ^ io.b

    io.s := axorb ^ io.cin
    io.cout := (io.a & io.b) || (axorb & io.cin)
}

object FA {
    def apply(a: Bool, b: Bool, cin: Bool) = {
        val fa = Module (new FA)
        fa.io.a := a
        fa.io.b := b
        fa.io.cin := cin

        (fa.io.s, fa.io.cout)
    }
}

class CarrySaveAdder(val width: Int) extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(width.W))
        val b = Input(UInt(width.W))
        val c = Input(UInt(width.W))

        val S = Output(UInt(width.W))
        val C = Output(UInt(width.W))
    })

    val Ss = Wire(Vec(width, Bool()))
    val Cs = Wire(Vec(width, Bool()))

    for (i <- 0 until width) {
        val (si , ci ) = FA(io.a(i), io.b(i), io.c(i))
        Ss(i) := si
        Cs(i) := ci
    }

    io.S := Ss.asUInt
    io.C := Cs.asUInt<<1
}

object CarrySaveAdder {
    def apply (width: Int) (a: UInt, b: UInt, c: UInt) = {
        val csa = Module (new CarrySaveAdder(width))
        csa.io.a := a
        csa.io.b := b
        csa.io.c := c

        (csa.io.S, csa.io.C)
    }
}
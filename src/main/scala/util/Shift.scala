package fpu

import chisel3._
import chisel3.util._

// refer https://www.princeton.edu/~rblee/ELE572Papers/Fall04Readings/Shifter_Schulte.pdf

class ShiftRight(val width: Int) extends Module {
    val shiftw = log2Up(width)
    val io = IO(new Bundle {
        val a = Input(UInt(width.W))
        val shift = Input(UInt(shiftw.W))

        val out = Output(UInt(width.W))
    })

    def tree(data: UInt, shift: UInt) : UInt = {
        def nextLayer(prev: UInt, idx: Int) : UInt = {
            if (idx < 0) {
                prev
            } else {
                nextLayer(Mux(shift(idx), prev >> (1<<idx), prev), idx-1)
            }
        }

        nextLayer(data, shiftw-1)
    }

    io.out := tree(io.a, io.shift)
}

object ShiftRight {
    def apply (width: Int) (a: UInt, shift: UInt) : UInt = {
        val mod = Module(new ShiftRight(width))
        mod.io.a := a
        mod.io.shift := shift

        mod.io.out
    }
}

class ShiftLeft(val width: Int) extends Module {
    val shiftw = log2Up(width)
    val io = IO(new Bundle {
        val a = Input(UInt(width.W))
        val shift = Input(UInt(shiftw.W))

        val out = Output(UInt(width.W))
    })

    def tree(data: UInt, shift: UInt) : UInt = {
        def nextLayer(prev: UInt, idx: Int) : UInt = {
            if (idx < 0) {
                prev
            } else {
                nextLayer(Mux(shift(idx), prev << (1<<idx), prev), idx-1)
            }
        }

        nextLayer(data, shiftw-1)
    }

    io.out := tree(io.a, io.shift)
}

object ShiftLeft {
    def apply (width: Int) (a: UInt, shift: UInt) : UInt = {
        val mod = Module(new ShiftLeft(width))
        mod.io.a := a
        mod.io.shift := shift

        mod.io.out
    }
}

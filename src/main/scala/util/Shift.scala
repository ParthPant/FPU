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
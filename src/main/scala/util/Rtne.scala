package fpu

import chisel3._

// Refer to this SO https://stackoverflow.com/questions/8981913/how-to-perform-round-to-even-with-floating-point-numbers

class Rtne(val inSize:Int, val expSize:Int) extends Module { 
    val outSize = inSize - 3

    val io = IO(new Bundle {
        val unrounded = Input(UInt(inSize.W))
        val expIn = Input(UInt(expSize.W))
        val rounded = Output(UInt(outSize.W))
        val expOut = Output(UInt(expSize.W))
    })

    val G = io.unrounded(2).asBool
    val R = io.unrounded(1).asBool
    val S = io.unrounded(0).asBool

    val unroundedWire = Wire(Vec(outSize, UInt(1.W)))
    for (i <- 0 until outSize) {
        unroundedWire(i) := io.unrounded(3+i)
    }

    when (!G) {
        io.rounded := unroundedWire.asUInt
        io.expOut := io.expIn
    } .otherwise {
        when (!(R|S) && !io.unrounded(3).asBool) {
            io.rounded := unroundedWire.asUInt
            io.expOut := io.expIn
        } .otherwise {
            io.rounded := unroundedWire.asUInt + 1.U
            when (io.rounded === 0.U) {
                io.expOut := io.expIn + 1.U
            } .otherwise {
                io.expOut := io.expIn
            }
        }
    }
}
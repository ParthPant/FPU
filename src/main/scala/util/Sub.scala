package fpu

import chisel3._

class FastSubtractorPipelined(val width: Int, val valency: Int) extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(width.W))
        val b = Input(UInt(width.W))
        val cin = Input(UInt(1.W))

        val Diff = Output(UInt(width.W))
        val Cout = Output(UInt(1.W))
    })

    val (diff, cout) = FastAdderPipelined(width, valency)(~io.a, io.b, io.cin)

    io.Diff := ~diff
    io.Cout := cout
}

object FastSubtractorPipelined {
    def apply (width: Int, valency: Int) (a: UInt, b: UInt, cin: UInt) : (UInt, UInt) = {
        val mod = Module (new FastSubtractorPipelined(width, valency))
        mod.io.a := a
        mod.io.b := b
        mod.io.cin := cin

        (mod.io.Diff, mod.io.Cout)
    }
}

package fpu

import chisel3._

class FastSubtractorPipelined(val width: Int) extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(width.W))
        val b = Input(UInt(width.W))
        val cin = Input(UInt(1.W))

        val Diff = Output(UInt(width.W))
        val Cout = Output(UInt(1.W))
    })

    val (diff, cout) = FastAdderPipelined(width)(~io.a, io.b, io.cin)

    io.Diff := ~diff
    io.Cout := cout
}

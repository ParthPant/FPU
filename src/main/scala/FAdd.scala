package fpu

import chisel3._
import chisel3.util._
import scala.math.pow

class FAdd extends Module {
    val io = IO(new Bundle {
        val x = Input(new FloatingPoint)
        val y = Input(new FloatingPoint)
        val out = Output(new FloatingPoint)
    })


    val closePath = Module(new ClosePath) 
    closePath.io.x := io.x 
    closePath.io.y := io.y 

    io.out := closePath.io.out
}

class ClosePath extends Module {
    val io = IO(new Bundle {
        val x = Input(new FloatingPoint)
        val y = Input(new FloatingPoint)
        val out = Output(new FloatingPoint)
    })

    val xgty = io.x.exp >=io.y.exp
    val y0 = Mux(xgty, io.y, io.x)
    val x = Mux(xgty, io.x, io.y)

    val y = FloatingPoint(y0.sign, ShiftRight(24)(y0.mant, x.exp - y0.exp), x.exp)

    val (sum, cout) = FastAdderPipelined(24)(x.mant, y.mant, 0.U)
    val out = Wire(new FloatingPoint)

    out.sign := 0.U
    when (cout.asBool) {
        out.mant := Cat(1.U, sum(23, 1))
        out.exp := x.exp + 1.U
    } .otherwise {
        out.mant := sum
        out.exp := x.exp
    }

    io.out := out
}

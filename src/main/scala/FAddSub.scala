package fpu

import chisel3._
import chisel3.util._
import scala.math.pow

class FAddSub extends Module {
    val io = IO(new Bundle {
        val a = Input(new FloatingPoint)
        val b = Input(new FloatingPoint)
        val o = Input(Bool())
        val out = Output(new FloatingPoint)
    })

    val a = io.a
    val b = FloatingPoint(io.o ^ io.b.sign, io.b.significand, io.b.exp)
    val agtb = io.a.exp >= io.b.exp

    val x = Mux(agtb, a, b)
    val _y = Mux(agtb, b, a)
    val y = FloatingPoint(_y.sign, ShiftRight(24)(_y.significand, x.exp - _y.exp), x.exp)

    val sz = x.sign ^ y.sign

    val add = FastAdderPipelined(24)(x.significand, y.significand, 0.U)
    val sub = FastAdderPipelined(24)(x.significand, ~y.significand, 1.U)

    val sum = Mux(sz.asBool, sub._1, add._1)
    val cout = Mux(sz.asBool, sub._2, add._2)

    io.out.sign := Mux(sz.asBool, x.sign ^ ~cout.asBool, x.sign)

    when (~sz.asBool) {
        when (cout.asBool) {
            io.out.significand := Cat(1.U, sum(23, 1))
            io.out.exp := x.exp + 1.U
        } .otherwise {
            val clz = Module(new CLZ24)
            clz.io.in := sum

            val shifter = Module(new ShiftLeft(24))
            shifter.io.a := sum
            shifter.io.shift := clz.io.Z

            io.out.significand := shifter.io.out
            io.out.exp := x.exp - clz.io.Z
        }
    } .otherwise {
        val s = ~sum + 1.U
        when (~cout.asBool) {
            val clz = Module(new CLZ24)
            clz.io.in := s

            val shifter = Module(new ShiftLeft(24))
            shifter.io.a := s
            shifter.io.shift := clz.io.Z

            io.out.significand := shifter.io.out
            io.out.exp := x.exp - clz.io.Z
        } .otherwise {
            val clz = Module(new CLZ24)
            clz.io.in := sum

            val shifter = Module(new ShiftLeft(24))
            shifter.io.a := sum
            shifter.io.shift := clz.io.Z

            io.out.significand := shifter.io.out
            io.out.exp := x.exp - clz.io.Z
        }
    }
}

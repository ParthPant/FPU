package fpu

import chisel3._
import chisel3.util._

// Refer: https://www.giscafe.com/book/ASIC/CH02/CH02.16.php

class ArrayMultiplier(val width : Int) extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(width.W))
        val b = Input(UInt(width.W))

        val P = Output(UInt((2*width).W))
    })

    val as = io.a.asBools
    val bs = io.b.asBools

    val pl = Wire(Vec(width, Bool()))
    val ph = Wire(Vec(width, Bool()))

    def makeTree(a: UInt, b: UInt) : (UInt, UInt) = {
        def nextLayer(prev: (UInt, UInt), depth: Int) : (UInt, UInt) = {
            if (depth < width) {
                val next = Seq.tabulate(width)(n => FA(if (n < width-1) prev._1(n+1) else 0.B, a(n) & b(depth), prev._2(n)))
                val s = VecInit(next.map(_._1)).asUInt
                val c = VecInit(next.map(_._2)).asUInt
                pl(depth) := s(0)
                nextLayer((s, c), depth+1)
            } else {
                val x = Cat(0.B, prev._1(width-1, 1))
                val y = prev._2
                val (s, c) = RippleCarryAdder(width)(x, y, 0.U) 
                ph := s.asBools
                (s, c) 
            }
        }
        nextLayer((0.U,0.U), 0)
    }

    makeTree(io.a, io.b)

    io.P := Cat(ph.asUInt, pl.asUInt)
}

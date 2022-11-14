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

    def makeTree(a: Seq[Bool], b: Seq[Bool]) : Seq[(Bool, Bool)] = {
        def nextLayer(prev: Seq[(Bool, Bool)], depth: Int) : Seq[(Bool, Bool)] = {
            if (depth < width) {
                val next = Seq.tabulate(width)(n => FA(if (n < width-1) prev(n+1)._1 else 0.B, a(n) & b(depth), prev(n)._2))
                pl(depth) := next(0)._1
                nextLayer(next, depth+1)
            } else {
                val x = Cat(0.B, VecInit(prev.drop(1).map(_._1)).asUInt)
                val y = VecInit(prev.map(_._2)).asUInt
                val (s, c) = RippleCarryAdder(width)(x, y, 0.U) 
                ph := s.asBools
                s.asBools.zip(c.asBools)
            }
        }
        nextLayer(Seq.fill(width)((0.B,0.B)), 0)
    }

    makeTree(as, bs)

    io.P := Cat(ph.asUInt, pl.asUInt)
}

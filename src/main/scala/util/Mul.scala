package fpu

import chisel3._
import chisel3.util._

// Refer: https://www.giscafe.com/book/ASIC/CH02/CH02.16.php

class ArrayMultiplier(val width : Int, val stages: Seq[Int]) extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(width.W))
        val b = Input(UInt(width.W))

        val P = Output(UInt((2*width).W))
    })

    def makeTree(a: UInt, b: UInt) : UInt = {
        def nextLayer(ops: (UInt, UInt), prev: (UInt, UInt), res: UInt, depth: Int) : UInt = {
            if (depth < width) {
                val (a, b) = ops
                val next = Seq.tabulate(width)(n => FA(if (n < width-1) prev._1(n+1) else 0.B, a(n) & b(0), prev._2(n)))
                val nextb = if (depth < width-1) b(width-depth-1, 1) else 0.U
                val s = VecInit(next.map(_._1)).asUInt
                val c = VecInit(next.map(_._2)).asUInt
                val nextres = VecInit.tabulate(depth+1)(n => {
                    if (n == depth) {
                        s(0) 
                    } else {
                        res(n)
                    }
                }).asUInt

                if (stages contains depth) {
                    nextLayer((RegNext(a), RegNext(nextb)), (RegNext(s), RegNext(c)), RegNext(nextres), depth+1)
                } else {
                    nextLayer((a, nextb), (s, c), nextres, depth+1)
                }
            } else {
                val x = Cat(0.B, prev._1(width-1, 1))
                val y = prev._2
                val (s, c) = RippleCarryAdder(width)(x, y, 0.U) 
                val product = Cat(s, res)
                product
            }
        }

        nextLayer((a, b), (0.U,0.U), 0.U, 0)
    }

    io.P := makeTree(io.a, io.b)
}

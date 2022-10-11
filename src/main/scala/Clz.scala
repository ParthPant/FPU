package fpu

import chisel3._
import chisel3.util._

class NLC extends Module {
    val io = IO(new Bundle {
        val in = Input(UInt(4.W))
        val Z = Output(UInt(2.W))
        val a = Output(UInt(1.W))
    })

    val ins = io.in.asBools
    val Zs = VecInit(~((~io.in(2) & io.in(1)) || io.in(3)), ~(io.in(2) || io.in(3)))

    io.a := ~(ins.reduce(_ || _))
    io.Z := Zs.asUInt
}

class CLZ32 extends Module {
    val io = IO(new Bundle {
        val in = Input(UInt(32.W))
        val Z = Output(UInt(5.W))
        val a = Output(UInt(1.W))
    })

    val ins = io.in.asBools

    val NLCs = for (i <- 0 until 8) yield {
        Module (new NLC)
    }

    val zs = Wire(Vec(8, UInt(2.W)))
    val as = Wire(Vec(8, Bool()))

    for (i <- 0 until 8) {
        val j = 7 - i
        NLCs(i).io.in := VecInit(ins.slice(4*j, 4*j+4)).asUInt     
        zs(i) := NLCs(i).io.Z
        as(i) := ~(NLCs(i).io.a.asBool)
    }

    val BNEout = PriorityEncoder(as.asUInt)
    val MUXout = PriorityMux(as.zip(zs))

    val out = Cat(BNEout, MUXout)

    io.a := out(4)
    io.Z := VecInit(out.asBools.slice(0, 4)).asUInt
}

class CLZ64 extends Module {
    val io = IO(new Bundle {
        val in = Input(UInt(64.W))
        val Z = Output(UInt(6.W))
        val a = Output(UInt(1.W))
    })

    val ins = io.in.asBools

    val CLZ32H = Module (new CLZ32)
    val CLZ32L = Module (new CLZ32)

    CLZ32L.io.in := VecInit(ins.slice(0, 32)).asUInt
    CLZ32H.io.in := VecInit(ins.slice(32, 64)).asUInt

    io.a := CLZ32H.io.a & CLZ32L.io.a
    io.Z := Cat(CLZ32H.io.a, Mux(CLZ32H.io.a.asBool, CLZ32L.io.Z, CLZ32H.io.Z))
}